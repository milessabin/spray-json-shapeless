package com.ensime.server

import scala.tools.nsc.interactive.{Global, CompilerControl}
import scala.tools.nsc.util.{SourceFile, Position, OffsetPosition}
import scala.tools.nsc.util.NoPosition
import scala.tools.nsc.symtab.Types
import scala.tools.nsc.symtab.Symbols
import com.ensime.util.SExp._
import com.ensime.util.{SExp, SExpable}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.collection.mutable.{ HashMap, HashEntry, HashSet }
import scala.collection.mutable.{ ArrayBuffer, SynchronizedMap,LinkedHashMap }


class Note(file:String, msg:String, severity:Int, beg:Int, end:Int, line:Int, col:Int) extends SExpable{

  private val tmp = "" + file + msg + severity + beg + end + line + col;
  override val hashCode = tmp.hashCode

  def toSExp() = {
    SExp(
      key(":severity"), friendlySeverity,
      key(":msg"), msg,
      key(":beg"), beg,
      key(":end"), end,
      key(":line"), line,
      key(":col"), col,
      key(":file"), file
    )
  }

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

