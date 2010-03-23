package com.ensime.server

import scala.tools.nsc.interactive.{Global, CompilerControl}
import scala.tools.nsc.{Settings}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.actors._  
import scala.actors.Actor._  
import scala.tools.nsc.io.{AbstractFile}
import scala.tools.nsc.util.{SourceFile, Position}
import scala.tools.nsc.reporters.Reporter
import scala.concurrent.SyncVar


case class CompilationResult(notes:List[Note])
case class ReloadFile(file:String)

class Compiler(project:Project) extends Actor{

  val settings:Settings = new Settings(Console.println)
  settings.processArguments(
    List(
      "-cp", ".",
      "-verbose"
    ),
    false
  )
  val reporter:PresentationReporter = new PresentationReporter()

  class PresentationCompiler(settings:Settings, reporter:Reporter) extends Global(settings,reporter){
  }
  val nsc = new PresentationCompiler(settings, reporter)

  def act() {
    println("Compiler starting..")
    nsc.newRunnerThread
    loop {
      receive {
	case ReloadFile(path:String) => 
	{
	  println("Compiler: Got reload request...")
	  reporter.clearNotes
	  val f:SourceFile = nsc.getSourceFile(path)
	  val x = new nsc.Response[Unit]()
	  nsc.askReload(List(f), x)
	  x.get
	  println("Compiler: Finished reload.")
	  println("Notes: " + reporter.allNotes)
	  project ! CompilationResult(reporter.allNotes)
	}
      }
    }
  }
}


class Note(file:String, msg:String, severity:Int, beg:Int, end:Int, line:Int, col:Int){

  private val str = "" + file + msg + severity + beg + end + line + col;
  override val hashCode = str.hashCode

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
    println(notes.toString)
    notes.flatMap{ e => e._2 }.toList
  }
  
  def clearNotes{
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
