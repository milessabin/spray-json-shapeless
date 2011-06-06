package org.ensime.util

import scala.collection.mutable.{HashEntry, HashMap, HashSet, SynchronizedMap}
import scala.tools.nsc.interactive.CompilerControl
import scala.tools.nsc.reporters.{ConsoleReporter, Reporter}
import scala.tools.nsc.util.{OffsetPosition, Position, SourceFile}

trait ReportHandler {
  def messageUser(str:String) {}
  def clearAllScalaNotes(){}
  def clearAllJavaNotes(){}
  def clearScalaNotes(filenames: List[String]){}
  def clearJavaNotes(filenames: List[String]){}
  def reportScalaNotes(notes:List[Note]){}
  def reportJavaNotes(notes:List[Note]){}
}

class PresentationReporter(handler:ReportHandler) extends Reporter {

  private val notes = new HashMap[SourceFile, HashSet[Note]] with SynchronizedMap[SourceFile, HashSet[Note]] {
    override def default(k: SourceFile) = { val v = new HashSet[Note]; put(k, v); v }
  }

  def allNotes: Iterable[Note] = notes.flatMap { e => e._2 }.toList

  private var enabled = true
  def enable(){ enabled = true }
  def disable(){ enabled = false }

  override def reset {
    super.reset
    notes.clear
    handler.clearAllScalaNotes()
  }

  override def info(pos: Position, msg: String, force: Boolean) {
    if(msg.contains("MissingRequirementError: object scala not found") || 
      msg.contains("MissingRequirementError: class scala.runtime.BooleanRef not found")){
      handler.messageUser("Fatal Error: Scala language library not found on classpath. You may need to run 'sbt update', or 'mvn update'.")
    }
    println("INFO: " + msg)
  }

  override def info0(pos: Position, msg: String, severity: Severity, force: Boolean) {
    severity.count += 1
    try {
      if (pos.isDefined) {
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
	if(enabled){
	  handler.reportScalaNotes(List(note))
	}
      }
    } catch {
      case ex: UnsupportedOperationException => {}
    }
  }

  def formatMessage(msg: String): String = {
    augmentString(msg).map {
      case '\n' => ' '
      case '\r' => ' '
      case c => c
    }
  }

}

