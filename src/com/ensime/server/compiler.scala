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
import java.io.File
import com.ensime.server.SExp._
import scala.tools.nsc.ast._

case class CompilationResultEvent(notes:List[Note])
case class TypeCompletionResultEvent(members:List[MemberInfo], callId:Int)
case class InspectTypeResultEvent(info:TypeInspectInfo, callId:Int)
case class ReloadFileEvent(file:File)
case class RemoveFileEvent(file:File)
case class ScopeCompletionEvent(file:File, point:Int)
case class TypeCompletionEvent(file:File, point:Int, prefix:String, callId:Int)
case class InspectTypeEvent(file:File, point:Int, callId:Int)

case class BackgroundCompileCompleteEvent()

class Compiler(project:Project, config:ProjectConfig) extends Actor{

  val rootDir = new File(config.rootDir)
  val cpPieces = config.classpath.split(":")
  val cpFiles = cpPieces.map{ s =>
    val f = new File(rootDir, s)
    f.getAbsolutePath()
  }
  val srcDir = new File(rootDir, config.srcDir)

  val args = List(
    "-cp", cpFiles.mkString(":"),
    "-verbose",
    "-sourcepath", srcDir.getAbsolutePath,
    config.srcFiles.split(":").mkString(" ")
  )

  val settings:Settings = new Settings(Console.println)
  settings.processArguments(args, false)
  val reporter:PresentationReporter = new PresentationReporter()


  class PresentationCompiler(settings:Settings, reporter:Reporter, parent:Actor) extends Global(settings,reporter){

    /**
    * Override so we send a notification to compiler actor when finished..
    */
    override def recompile(units: List[RichCompilationUnit]) {
      super.recompile(units)
      parent ! BackgroundCompileCompleteEvent()
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
      val maybeType:Option[Type] = x1.get match{
	case Left(t) => Some(t.tpe)
	case Right(e) => None
      }

      // Then grab the members of that type
      val x2 = new nsc.Response[List[Member]]()
      askTypeCompletion(p, x2)
      val members:List[Member] = x2.get match{
	case Left(m) => m
	case Right(e) => List()
      }
      // ...filtering out non-visible members
      val visMembers = (members.flatMap {
	  case TypeMember(sym, tpe, true, _, _) => {
	    List(MemberInfo(sym.nameString, tpe.toString))
	  }
	  case _ => List()
	}).sortWith((a,b) => a.name <= b.name)

      // Then return all the info about this type..
      maybeType match{
	case Some(tpe) => {
	  val typeSym = tpe.typeSymbol
	  val typeInfo = TypeInfo(tpe.toString, typeSym.pos)
	  TypeInspectInfo(typeInfo, visMembers)
	}
	case None => {
	  TypeInspectInfo.nullInspectInfo
	}
      }
    }

    def completeMemberAt(p: Position, prefix:String):List[MemberInfo] = {
      blockingQuickReload(p.source)
      val x2 = new Response[List[Member]]()
      askTypeCompletion(p, x2)
      val members:List[Member] = x2.get match{
	case Left(m) => m
	case Right(e) => List()
      }
      val visMembers = members.flatMap{
	case TypeMember(sym, tpe, true, _, _) => {
	  if(sym.nameString.startsWith(prefix)){
	    List(MemberInfo(sym.nameString, tpe.toString))
	  }
	  else{
	    List()
	  }
	}
	case _ => List()
      }.sortWith((a,b) => a.name <= b.name)
      visMembers
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
    println("Compiler starting..")
    nsc.newRunnerThread
    nsc.askReset
    loop {
      try{
	receive {
	  case ReloadFileEvent(file:File) => 
	  {
	    val f:SourceFile = nsc.getSourceFile(file.getAbsolutePath())
	    nsc.blockingFullReload(f)
	    project ! CompilationResultEvent(reporter.allNotes)
	  }

	  case RemoveFileEvent(file:File) => 
	  {
	    val f:SourceFile = nsc.getSourceFile(file.getAbsolutePath())
	    nsc.removeUnitOf(f)
	  }

	  case ScopeCompletionEvent(file:File, point:Int) => 
	  {
	    val f:SourceFile = nsc.getSourceFile(file.getAbsolutePath())
	    val p:Position = new OffsetPosition(f, point);
	    val x = new nsc.Response[List[nsc.Member]]()
	    nsc.askScopeCompletion(p, x)
	    val members:List[nsc.Member] = x.get match{
	      case Left(m) => m
	      case Right(e) => List()
	    }
	  }

	  case TypeCompletionEvent(file:File, point:Int, prefix:String, callId:Int) => 
	  {
	    val f:SourceFile = nsc.getSourceFile(file.getAbsolutePath())
	    val p:Position = new OffsetPosition(f, point)
	    val members = nsc.completeMemberAt(p, prefix)
	    project ! TypeCompletionResultEvent(members, callId)
	  }

	  case InspectTypeEvent(file:File, point:Int, callId:Int) => 
	  {
	    val f:SourceFile = nsc.getSourceFile(file.getAbsolutePath())
	    val p:Position = new OffsetPosition(f, point)
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
	  System.err.println("Error at Compiler message loop: " + e)
	}
      }
    }
  }
}


case class MemberInfo(name:String, tpe:String){
  def toEmacsSExp = {
    SExp(
      ":name", name,
      ":type", tpe
    )
  }
}

case class TypeInspectInfo(tpe:TypeInfo, members:Iterable[MemberInfo]){
  def toEmacsSExp = SExp(
    ":type", tpe.toEmacsSExp,
    ":members", SExp(members.map{_.toEmacsSExp})
  )
}
object TypeInspectInfo{
  def nullInspectInfo() = {
    TypeInspectInfo(TypeInfo("NA", NoPosition), List())
  }
}

case class TypeInfo(name:String, pos:Position){
  def toEmacsSExp = SExp(
    ":name", name,
    ":file", if(pos.isDefined) { pos.source.path } else { 'nil },
    ":offset", if(pos.isDefined) { pos.point } else { 'nil }
  )
}


class Note(file:String, msg:String, severity:Int, beg:Int, end:Int, line:Int, col:Int){

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

  def toEmacsSExp = {
    SExp(
      ":severity", friendlySeverity,
      ":msg", msg,
      ":beg", beg,
      ":end", end,
      ":line", line,
      ":col", col,
      ":file", file
    )
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
