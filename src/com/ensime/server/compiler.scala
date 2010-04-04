package com.ensime.server

import scala.tools.nsc.interactive.{Global, CompilerControl}
import scala.tools.nsc.{Settings}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.actors._  
import scala.actors.Actor._  
import scala.tools.nsc.io.{AbstractFile}
import scala.tools.nsc.util.{SourceFile, Position, OffsetPosition}
import scala.tools.nsc.util.NoPosition
import scala.tools.nsc.reporters.Reporter
import scala.concurrent.SyncVar
import scala.collection.{Iterable, Map}
import java.io.File
import scala.tools.nsc.ast._
import com.ensime.util.RichFile._ // this makes implicit toRichFile active
import scala.tools.nsc.symtab.Types


case class CompilationResultEvent(notes:List[Note])
case class TypeCompletionResultEvent(members:List[MemberInfoLight], callId:Int)
case class InspectTypeResultEvent(info:TypeInspectInfo, callId:Int)
case class ReloadFileEvent(file:File)
case class RemoveFileEvent(file:File)
case class ScopeCompletionEvent(file:File, point:Int)
case class TypeCompletionEvent(file:File, point:Int, prefix:String, callId:Int)
case class InspectTypeEvent(file:File, point:Int, callId:Int)
case class CompilerShutdownEvent()

case class BackgroundCompileCompleteEvent()

class Compiler(project:Project, config:ProjectConfig) extends Actor{

  val rootDir = new File(config.rootDir)
  val cpFiles = config.classpath.map{ s =>
    val f = new File(rootDir, s)
    f.getAbsolutePath
  }
  val srcFiles = (
    for(s <- config.srcDirs;
      val srcRoot = new File(rootDir, s);
      f <- srcRoot.andTree if (f.getName.endsWith(".scala") && !f.isHidden))
    yield{
      f.getAbsolutePath
    })
  val args = List(
    "-cp", cpFiles.mkString(":"),
    "-verbose",
    srcFiles.mkString(" ")
  )

  println("Processed compiler args:" + args)

  val settings = new Settings(Console.println)
  settings.processArguments(args, false)
  val reporter = new PresentationReporter()

  class PresentationCompiler(settings:Settings, reporter:Reporter, parent:Actor) extends Global(settings,reporter){

    /**
    * Override so we send a notification to compiler actor when finished..
    */
    override def recompile(units: List[RichCompilationUnit]) {
      super.recompile(units)
      parent ! BackgroundCompileCompleteEvent()
    }

    def blockingReloadAll() {
      val all = srcFiles.map(nsc.getSourceFile(_)).toList
      val x = new Response[Unit]()
      askReload(all, x)
      x.get
    }    
    /** 
    *  Make sure a set of compilation units is loaded and parsed,
    *  but do not trigger a full recompile.
    */
    private def quickReload(sources: List[SourceFile], result: Response[Unit]) {
      respond(result)(reloadSources(sources))
    }

    /** 
    *  Make sure a set of compilation units is loaded and parsed,
    *  but do not trigger a full recompile.
    *  Return () to syncvar `result` on completion.
    */
    def askQuickReload(sources: List[SourceFile], result: Response[Unit]) = 
    scheduler postWorkItem new WorkItem {
      def apply() = quickReload(sources, result)
      override def toString = "quickReload " + sources
    }


    def inspectTypeAt(p: Position):TypeInspectInfo = {

      blockingQuickReload(p.source)

      // First grab the type at position
      val x1 = new Response[Tree]()
      askTypeAt(p, x1)
      x1.get match{
	case Left(tree) => {

	  // Get the type at position..
	  val tpe = tree.tpe
	  val typeInfo = TypeInfo(tpe)

	  // Then grab the members of that type
	  //
	  // TODO - shouldn't do another blocking 
	  // call here; we already have tpe..
	  val x2 = new nsc.Response[List[Member]]()
	  askTypeCompletion(p, x2)
	  val members = x2.get match{
	    case Left(m) => m
	    case Right(e) => List()
	  }

	  // ...filtering out non-visible and non-type members
	  val visMembers:List[TypeMember] = members.flatMap { 
	    case m@TypeMember(sym, tpe, true, _, _) => List(m)
	    case _ => List()
	  }

	  // create a list of pairs [(type, members-of-type)]
	  // ..sort the pairs on the subtype relation
	  val membersByOwner = visMembers.groupBy{
	    case TypeMember(sym, tpe, _, _, _) => {
	      val ownerSym = sym.owner
	      val ownerType = ownerSym.tpe
	      ownerType
	    }
	  }.iterator.toList.sortWith{
	    case ((t1,_),(t2,_)) => t1 <:< t2
	  }

	  // transform to [(type-info, member-infos-of-type)]..
	  val memberInfosByOwnerInfo = membersByOwner.map{
	    case (ownerTpe, members) => {
	      val memberInfos = members.map{
		case TypeMember(sym, tpe, _, _, _) => {	
		  val typeInfo = TypeInfo(tpe)
		  MemberInfo(sym.nameString, typeInfo, sym.pos)
		}
	      }.sortWith{(a,b) => a.name <= b.name}
	      (TypeInfo(ownerTpe), memberInfos)
	    }
	  }
	  
	  TypeInspectInfo(typeInfo, memberInfosByOwnerInfo)
	}
	case Right(e) => {
	  TypeInspectInfo.nullInspectInfo
	}
      }
    }


    def completeMemberAt(p: Position, prefix:String):List[MemberInfoLight] = {
      blockingQuickReload(p.source)
      val x2 = new Response[List[Member]]()
      askTypeCompletion(p, x2)
      val members = x2.get match{
	case Left(m) => m
	case Right(e) => List()
      }
      val visibleMembers = members.flatMap{
	case TypeMember(sym, tpe, true, _, _) => {
	  if(sym.nameString.startsWith(prefix)){
	    List(MemberInfoLight(sym.nameString, tpe.toString))
	  }
	  else{
	    List()
	  }
	}
	case _ => List()
      }.sortWith((a,b) => a.name <= b.name)
      visibleMembers
    }

    def blockingQuickReload(f:SourceFile){
      val x = new nsc.Response[Unit]()
      nsc.askQuickReload(List(f), x)
      x.get
    }

    def blockingFullReload(f:SourceFile){
      val x = new nsc.Response[Unit]()
      nsc.askReload(List(f), x)
      x.get
    }

  }

  val nsc = new PresentationCompiler(settings, reporter, this)

  def act() {
    nsc.newRunnerThread
    project ! SendBackgroundMessageEvent("Compiler is parsing sources...")
    nsc.blockingReloadAll
    project ! SendBackgroundMessageEvent("Compiler finished parsing!")
    loop {
      try{
	receive {

	  case CompilerShutdownEvent => 
	  {
	    nsc.askShutdown()
	    exit('stop)
	  }

	  case ReloadFileEvent(file:File) => 
	  {
	    val f = nsc.getSourceFile(file.getAbsolutePath())
	    nsc.blockingFullReload(f)
	    project ! CompilationResultEvent(reporter.allNotes)
	  }

	  case RemoveFileEvent(file:File) => 
	  {
	    val f = nsc.getSourceFile(file.getAbsolutePath())
	    nsc.removeUnitOf(f)
	  }

	  case ScopeCompletionEvent(file:File, point:Int) => 
	  {
	    val f = nsc.getSourceFile(file.getAbsolutePath())
	    val p = new OffsetPosition(f, point)
	    val x = new nsc.Response[List[nsc.Member]]()
	    nsc.askScopeCompletion(p, x)
	    val members = x.get match{
	      case Left(m) => m
	      case Right(e) => List()
	    }
	  }

	  case TypeCompletionEvent(file:File, point:Int, prefix:String, callId:Int) => 
	  {
	    val f = nsc.getSourceFile(file.getAbsolutePath())
	    val p = new OffsetPosition(f, point)
	    val members = nsc.completeMemberAt(p, prefix)
	    project ! TypeCompletionResultEvent(members, callId)
	  }

	  case InspectTypeEvent(file:File, point:Int, callId:Int) => 
	  {
	    val f = nsc.getSourceFile(file.getAbsolutePath())
	    val p = new OffsetPosition(f, point)
	    val inspectInfo = nsc.inspectTypeAt(p)
	    project ! InspectTypeResultEvent(inspectInfo, callId)
	  }

	  case BackgroundCompileCompleteEvent() => 
	  {
	    project ! CompilationResultEvent(reporter.allNotes)
	  }

	  case other => 
	  {
	    println("Compiler: WTF, what's " + other)
	  }
	}

      }
      catch{
	case e:Exception =>
	{
	  System.err.println("Error at Compiler message loop: " + e + " :\n" + e.getStackTraceString)
	}
      }
    }
  }
}

case class MemberInfo(name:String, tpe:TypeInfo, pos:Position){}
case class MemberInfoLight(name:String, tpeName:String){}
case class TypeInfo(name:String, declaredAs:scala.Symbol, generalName:String, pos:Position){}
object TypeInfo{
  def apply(tpe:Types#Type):TypeInfo = {
    val typeSym = tpe.typeSymbol
    val declAs = (
      if(typeSym.isTrait){
	'trait
      } 
      else if(typeSym.isInterface){
	'interface
      } 
      else if(typeSym.isClass){
	'class
      }
      else if(typeSym.isAbstractClass){
	'abstractclass
      }
      else{
	'nil
      }
    )
    TypeInfo(tpe.toString, declAs, typeSym.fullName, typeSym.pos)
  }
  def nullTypeInfo() = {
    TypeInfo("NA", 'class, "NA", NoPosition)
  }
}
case class TypeInspectInfo(tpe:TypeInfo, membersByOwner:Iterable[(TypeInfo, Iterable[MemberInfo])]){}
object TypeInspectInfo{
  def nullInspectInfo() = {
    TypeInspectInfo(TypeInfo.nullTypeInfo, Map())
  }
}

case class Note(file:String, msg:String, severity:Int, beg:Int, end:Int, line:Int, col:Int){

  private val tmp = "" + file + msg + severity + beg + end + line + col;
  override val hashCode = tmp.hashCode

  override def equals(other:Any):Boolean = {
    other match{
      case n:Note => n.hashCode == this.hashCode
      case _ => false
    }
  }

  def friendlySeverity = severity match {
    case 2 => 'error
    case 1 => 'warn
    case 0 => 'info
  }

}


import scala.collection.mutable.{ HashMap, HashEntry, HashSet }
import scala.collection.mutable.{ ArrayBuffer, SynchronizedMap }


class PresentationReporter extends Reporter {

  private val notes = new HashMap[SourceFile, HashSet[Note]] with SynchronizedMap[SourceFile, HashSet[Note]] {
    override def default(k : SourceFile) = { val v = new HashSet[Note] ; put(k, v); v }
  }

  def notesFor(file:SourceFile):List[Note] = {
    notes(file).toList
  }

  def allNotes():List[Note] = {
    notes.flatMap{ e => e._2 }.toList
  }
  
  override def reset{
    super.reset
    notes.clear
  }

  override def info(pos: Position, msg: String, force: Boolean){
    println("INFO: " + msg)
  }
  
  override def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
    severity.count += 1
    try {
      if(pos.isDefined) {
	val source = pos.source
	val note = new Note(
	  source.file.absolute.path,
	  formatMessage(msg),
	  severity.id,
	  pos.startOrPoint,
	  pos.endOrPoint,
	  pos.line,
	  pos.column
	)
	notes(source) += note
      }
    } catch {
      case ex : UnsupportedOperationException => 
    }
  }

  def formatMessage(msg:String):String = {
    augmentString(msg).map { 
      case '\n' => ' '
      case '\r' => ' '
      case c => c
    }
  }

}
