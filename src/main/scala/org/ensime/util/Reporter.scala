package org.ensime.util

import scala.collection.mutable.{ ArrayBuffer, SynchronizedMap, LinkedHashMap, HashMap, HashEntry, HashSet }
import scala.tools.nsc.interactive.{Global, CompilerControl}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.symtab.{Symbols, Types}
import scala.tools.nsc.util.{NoPosition, SourceFile, Position, OffsetPosition}


case class NoteList(notes:Iterable[Note])

class Note(val file:String, val msg:String, val severity:Int, val beg:Int, val end:Int, val line:Int, val col:Int){

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


class PresentationReporter extends Reporter {


  private val notes = new HashMap[SourceFile, HashSet[Note]] with SynchronizedMap[SourceFile, HashSet[Note]] {
    override def default(k : SourceFile) = { val v = new HashSet[Note] ; put(k, v); v }
  }

  def notesFor(file:SourceFile):NoteList = {
    NoteList(notes(file).toList)
  }

  def allNotes():NoteList = {
    NoteList(notes.flatMap{ e => e._2 }.toList)
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

