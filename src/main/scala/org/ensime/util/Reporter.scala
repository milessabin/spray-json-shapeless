/**
*  Copyright (c) 2010, Aemon Cannon
*  All rights reserved.
*
*  Redistribution and use in source and binary forms, with or without
*  modification, are permitted provided that the following conditions are met:
*      * Redistributions of source code must retain the above copyright
*        notice, this list of conditions and the following disclaimer.
*      * Redistributions in binary form must reproduce the above copyright
*        notice, this list of conditions and the following disclaimer in the
*        documentation and/or other materials provided with the distribution.
*      * Neither the name of ENSIME nor the
*        names of its contributors may be used to endorse or promote products
*        derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
*  DISCLAIMED. IN NO EVENT SHALL Aemon Cannon BE LIABLE FOR ANY
*  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package org.ensime.util

import scala.collection.mutable.{ HashEntry, HashMap, HashSet, SynchronizedMap }
import scala.tools.nsc.interactive.CompilerControl
import scala.tools.nsc.reporters.{ ConsoleReporter, Reporter }
import scala.tools.nsc.util.{ OffsetPosition, Position, SourceFile }

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

  private val notes = new HashMap[SourceFile, HashSet[Note]] with SynchronizedMap[SourceFile, HashSet[Note]] {
    override def default(k: SourceFile) = { val v = new HashSet[Note]; put(k, v); v }
  }

  def allNotes: Iterable[Note] = notes.flatMap { e => e._2 }.toList

  private var enabled = true
  def enable() { enabled = true }
  def disable() { enabled = false }

  override def reset {
    super.reset
    notes.clear
    if(enabled){
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
            pos.startOrPoint,
            pos.endOrPoint,
            pos.line,
            pos.column)
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

