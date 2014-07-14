package org.ensime.util

import scala.tools.nsc.reporters.Reporter
import scala.reflect.internal.util.Position

trait ReportHandler {
  def messageUser(str: String) {}
  def clearAllScalaNotes() {}
  def clearAllJavaNotes() {}
  def clearScalaNotes(filenames: List[String]) {}
  def clearJavaNotes(filenames: List[String]) {}
  def reportScalaNotes(notes: List[Note]) {}
  def reportJavaNotes(notes: List[Note]) {}
}

class PresentationReporter(handler: ReportHandler) extends Reporter {

  private var enabled = true
  def enable() { enabled = true }
  def disable() { enabled = false }

  override def reset() {
    super.reset()
    if (enabled) {
      handler.clearAllScalaNotes()
    }
  }

  override def info(pos: Position, msg: String, force: Boolean) {
    if (msg.contains("MissingRequirementError: object scala not found") ||
      msg.contains("MissingRequirementError: class scala.runtime.BooleanRef not found")) {
      handler.messageUser("Fatal Error: Scala language library not found on classpath. You may need to run 'sbt update', or 'mvn update'.")
    }
    println("INFO: " + msg)
  }

  override def info0(pos: Position, msg: String, severity: Severity, force: Boolean) {
    severity.count += 1
    try {
      if (enabled) {
        if (pos.isDefined) {
          val source = pos.source
          val f = source.file.absolute.path
          val note = new Note(
            f,
            formatMessage(msg),
            severity.id,
            pos.start,
            pos.end,
            pos.line,
            pos.column)
          handler.reportScalaNotes(List(note))
        }
      }
    } catch {
      case ex: UnsupportedOperationException =>
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

