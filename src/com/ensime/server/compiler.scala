package com.ensime.server

import scala.tools.nsc.interactive.{Global, CompilerControl}
import scala.tools.nsc.{Settings}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.actors._  
import scala.actors.Actor._  
import scala.tools.nsc.io.{AbstractFile}
import scala.tools.nsc.util.{SourceFile, Position, OffsetPosition}
import scala.tools.nsc.reporters.Reporter
import scala.concurrent.SyncVar
import java.io.File

case class CompilationResultEvent(notes:List[Note])
case class TypeCompletionResultEvent(members:List[AccessibleTypeMember], callId:SExp)
case class ReloadFileEvent(file:File)
case class RemoveFileEvent(file:File)
case class ScopeCompletionEvent(file:File, point:Int)
case class TypeCompletionEvent(file:File, point:Int, callId:SExp)

case class BackgroundCompileCompleteEvent()

class Compiler(project:Project, config:ProjectConfig) extends Actor{

  val settings:Settings = new Settings(Console.println)
  settings.processArguments(
    List(
      "-cp", config.classpath,
      "-verbose"
    ),
    false
  )
  val reporter:PresentationReporter = new PresentationReporter()

  class PresentationCompiler(settings:Settings, reporter:Reporter, parent:Actor) extends Global(settings,reporter){

    // Override and send notification to parent actor when finished..
    override def recompile(units: List[RichCompilationUnit]) {
      super.recompile(units)
      parent ! BackgroundCompileCompleteEvent()
    }

  }

  val nsc = new PresentationCompiler(settings, reporter, this)



  def act() {
    println("Compiler starting..")
    nsc.newRunnerThread
    loop {
      receive {
	case ReloadFileEvent(file:File) => 
	{
	  println("Compiler: Got reload request...")
	  val f:SourceFile = nsc.getSourceFile(file.getAbsolutePath())
	  val x = new nsc.Response[Unit]()
	  nsc.askReload(List(f), x)
	  x.get
	  println("Compiler: Finished reload.")
	  project ! CompilationResultEvent(reporter.allNotes)
	}
	case RemoveFileEvent(file:File) => 
	{
	  val f:SourceFile = nsc.getSourceFile(file.getAbsolutePath())
	  nsc.removeUnitOf(f)
	}
	case ScopeCompletionEvent(file:File, point:Int) => 
	{
	  println("Compiler: Got scope completion request...")
	  val f:SourceFile = nsc.getSourceFile(file.getAbsolutePath())
	  val p:Position = new OffsetPosition(f, point);
	  val x = new nsc.Response[List[nsc.Member]]()
	  nsc.askScopeCompletion(p, x)
	  val members:List[nsc.Member] = x.get match{
	    case Left(m) => m
	    case Right(e) => List()
	  }
	  println("Compiler: Finished scope completion.")
	  println(members)
	}

	case TypeCompletionEvent(file:File, point:Int, callId:SExp) => 
	{
	  println("Compiler: Got type completion request...")
	  val f:SourceFile = nsc.getSourceFile(file.getAbsolutePath())
	  val p:Position = new OffsetPosition(f, point);
	  val x = new nsc.Response[List[nsc.Member]]()
	  nsc.askTypeCompletion(p, x)
	  val members:List[nsc.Member] = x.get match{
	    case Left(m) => m
	    case Right(e) => List()
	  }
	  val visMembers = members.flatMap{ m => 
	    m match{
	      case nsc.TypeMember(sym, tpe, access, _, _) => {
		if(access){
		  List(new AccessibleTypeMember(sym.toString, tpe.toString))
		}
		else{
		  List()
		}
	      }
	      case nsc.ScopeMember(sym, tpe, _, _) => List()
	    }
	  }
	  project ! TypeCompletionResultEvent(visMembers, callId)
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
  }
}

class AccessibleTypeMember(name:String, tpe:String){
  def toEmacsSExp = {
    SExpList(List(
	KeywordAtom(":name"), StringAtom(name),
	KeywordAtom(":type"), StringAtom(tpe)
      ))
  }
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
    case 2 => "error"
    case 1 => "warn"
    case 0 => "info"
  }

  def toEmacsSExp = {
    SExpList(List(
	KeywordAtom(":severity"), SymbolAtom(friendlySeverity),
	KeywordAtom(":msg"), StringAtom(msg),
	KeywordAtom(":beg"), IntAtom(beg),
	KeywordAtom(":end"), IntAtom(end),
	KeywordAtom(":line"), IntAtom(line), 
	KeywordAtom(":col"), IntAtom(col),
	KeywordAtom(":file"), StringAtom(file)
      ))
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
	println("Adding note: " + note.toEmacsSExp)
	notes(source) += note
      }
    } catch {
      case ex : UnsupportedOperationException => 
    }
  }


  def formatMessage(msg : String) = msg.map {
    case '\n' => ' '
    case '\r' => ' '
    case c => c
  }.mkString("","","")
}
