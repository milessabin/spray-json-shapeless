/**
*  Copyright (C) 2010 Aemon Cannon
*
*  This program is free software; you can redistribute it and/or
*  modify it under the terms of the GNU General Public License as
*  published by the Free Software Foundation; either version 2 of
*  the License, or (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public
*  License along with this program; if not, write to the Free
*  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
*  MA 02111-1307, USA.
*/

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
	val f = source.file.absolute.path
        val note = new Note(
          f,
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

