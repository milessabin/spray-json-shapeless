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
import scala.collection.mutable.{ HashMap, HashEntry, HashSet }
import scala.collection.mutable.{ ArrayBuffer, SynchronizedMap,LinkedHashMap }
import scala.collection.immutable.TreeSet
import java.io.File
import scala.tools.nsc.ast._
import com.ensime.util.RichFile._ 
import scala.tools.nsc.symtab.Types
import com.ensime.server.model.EntityInfo
import com.ensime.server.model._

case class CompilationResultEvent(notes:List[Note])
case class TypeCompletionResultEvent(members:List[MemberInfoLight], callId:Int)
case class InspectTypeResultEvent(info:TypeInspectInfo, callId:Int)
case class TypeByIdResultEvent(info:TypeInfo, callId:Int)
case class TypeAtPointResultEvent(info:TypeInfo, callId:Int)
case class InspectTypeByIdResultEvent(info:TypeInspectInfo, callId:Int)
case class ReloadFileEvent(file:File)
case class RemoveFileEvent(file:File)
case class ScopeCompletionEvent(file:File, point:Int)
case class TypeCompletionEvent(file:File, point:Int, prefix:String, callId:Int)
case class InspectTypeEvent(file:File, point:Int, callId:Int)
case class InspectTypeByIdEvent(id:Int, callId:Int)
case class TypeByIdEvent(id:Int, callId:Int)
case class TypeAtPointEvent(file:File, point:Int, callId:Int)
case class CompilerShutdownEvent()

case class BackgroundCompileCompleteEvent()

class Compiler(project:Project, config:ProjectConfig) extends Actor{


  private val rootDir:File = new File(config.rootDir)

  private val classpathFiles:Set[String] = config.classpath.map{ s =>
    (new File(rootDir, s)).getAbsolutePath
  }.toSet

  private def isValidSourceFile(f:File):Boolean = {
    f.exists && !f.isHidden && (f.getName.endsWith(".scala") || f.getName.endsWith(".java"))
  }

  private def expandSource(srcList:Iterable[String]):Set[String] = {
    (for(
	s <- srcList;
	val files = (new File(rootDir, s)).andTree;
	f <- files if isValidSourceFile(f)
      )
      yield{
	f.getAbsolutePath
      }).toSet
  }


  val includeSrcFiles = expandSource(config.srcList)
  val excludeSrcFiles = expandSource(config.excludeSrcList)
  val srcFiles = includeSrcFiles -- excludeSrcFiles

  val args = List(
    "-cp", classpathFiles.mkString(":"),
    "-verbose",
    srcFiles.mkString(" ")
  )

  println("Compiler args: " + args)

  val settings = new Settings(Console.println)
  settings.processArguments(args, false)
  val reporter = new PresentationReporter()


  class PresentationCompiler(settings:Settings, reporter:Reporter, parent:Actor) extends Global(settings,reporter){

    private val typeCache = new HashMap[Int, Types#Type]
    private val typeCacheReverse = new HashMap[Types#Type, Int]
    def clearTypeCache(){ 
      typeCache.clear
      typeCacheReverse.clear
    }
    def typeById(id:Int):Option[Types#Type] = { 
      typeCache.get(id)
    }
    def cacheType(tpe:Types#Type):Int = {
      if(typeCacheReverse.contains(tpe)){
	typeCacheReverse(tpe)
      }
      else{
	val id = typeCache.size + 1
	typeCache(id) = tpe
	typeCacheReverse(tpe) = id
	id
      }
    }


    /**
    * Override so we send a notification to compiler actor when finished..
    */
    override def recompile(units: List[RichCompilationUnit]) {
      super.recompile(units)
      parent ! BackgroundCompileCompleteEvent()
      parent
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

    import analyzer.{SearchResult, ImplicitSearch}

    private def typePublicMembers(tpe:Type):List[TypeMember] = {
      val scope = new Scope
      val members = new LinkedHashMap[Symbol, TypeMember]
      def addTypeMember(sym: Symbol, pre: Type, inherited: Boolean, viaView: Symbol) {
	val symtpe = pre.memberType(sym)
	if (scope.lookupAll(sym.name) forall (sym => !(members(sym).tpe matches symtpe))) {
	  scope enter sym
	  members(sym) = new TypeMember(
	    sym,
	    symtpe,
	    sym.isPublic,
	    inherited,
	    viaView)
	}
      }
      for (sym <- tpe.decls){
	addTypeMember(sym, tpe, false, NoSymbol)
      }
      for (sym <- tpe.members){
	addTypeMember(sym, tpe, true, NoSymbol)
      }
      members.values.toList
    }

    def prepareSortedMemberInfo(members:List[Member]):Iterable[(TypeInfo, Iterable[MemberInfo])] = {
      // ...filtering out non-visible and non-type members
      val visMembers:List[TypeMember] = members.flatMap {
	case m@TypeMember(sym, tpe, true, _, _) => List(m)
	case _ => List()
      }

      // create a list of pairs [(type, members-of-type)]
      // ..sort the pairs on the subtype relation
      val membersByOwner = visMembers.groupBy{
	case TypeMember(sym, _, _, _, _) => {
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
	      val typeInfo = TypeInfo(tpe, cacheType)
	      new MemberInfo(sym.nameString, typeInfo, sym.pos)
	    }
	  }.sortWith{(a,b) => a.name <= b.name}
	  (TypeInfo(ownerTpe, cacheType), memberInfos)
	}
      }

      memberInfosByOwnerInfo
    }

    def inspectType(tpe:Types#Type):TypeInspectInfo = {
      new TypeInspectInfo(
	TypeInfo(tpe, cacheType), 
	prepareSortedMemberInfo(typePublicMembers(tpe.asInstanceOf[Type]))
      )
    }

    def inspectTypeAt(p: Position):TypeInspectInfo = {

      blockingQuickReload(p.source)

      // Grab the members at this position..
      val x2 = new nsc.Response[List[Member]]()
      askTypeCompletion(p, x2)
      val members:List[Member] = x2.get match{
	case Left(m) => m
	case Right(e) => List()
      }
      val preparedMembers = prepareSortedMemberInfo(members)

      // Grab the type at position..
      val x1 = new Response[Tree]()
      askTypeAt(p, x1)
      val typeInfo = x1.get match{
	case Left(tree) => {
	  TypeInfo(tree.tpe, cacheType)
	}
	case Right(e) => {
	  TypeInfo.nullTypeInfo
	}
      }
      new TypeInspectInfo(typeInfo, preparedMembers)
    }

    def getTypeAt(p: Position):TypeInfo = {
      // Grab the type at position..
      val x1 = new Response[Tree]()
      askTypeAt(p, x1)
      val typeInfo = x1.get match{
	case Left(tree) => {
	  TypeInfo(tree.tpe, cacheType)
	}
	case Right(e) => {
	  TypeInfo.nullTypeInfo
	}
      }
      typeInfo
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
	    List(new MemberInfoLight(sym.nameString, tpe.toString, cacheType(tpe)))
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
      val x = new Response[Unit]()
      nsc.askQuickReload(List(f), x)
      x.get
    }

    def blockingFullReload(f:SourceFile){
      val x = new Response[Unit]()
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
	    nsc.clearTypeCache
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

	  case InspectTypeByIdEvent(id:Int, callId:Int) =>
	  {
	    val inspectInfo = nsc.typeById(id) match{
	      case Some(tpe) => nsc.inspectType(tpe)
	      case None => TypeInspectInfo.nullInspectInfo
	    }
	    project ! InspectTypeResultEvent(inspectInfo, callId)
	  }

	  case TypeAtPointEvent(file:File, point:Int, callId:Int) =>
	  {
	    val f = nsc.getSourceFile(file.getAbsolutePath())
	    val p = new OffsetPosition(f, point)
	    val typeInfo = nsc.getTypeAt(p)
	    project ! TypeAtPointResultEvent(typeInfo, callId)
	  }

	  case TypeByIdEvent(id:Int, callId:Int) =>
	  {
	    val tpeInfo = nsc.typeById(id) match{
	      case Some(tpe) => TypeInfo(tpe, nsc.cacheType)
	      case None => TypeInfo.nullTypeInfo
	    }
	    project ! TypeByIdResultEvent(tpeInfo, callId)
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
