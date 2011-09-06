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

package org.ensime.server

import java.io.File
import org.ensime.config.ProjectConfig
import org.ensime.debug.ProjectDebugInfo
import org.ensime.protocol._
import org.ensime.util._
import scala.actors._
import scala.actors.Actor._
import scala.tools.nsc.{ Settings }
import scala.tools.refactoring.common.Change
import scala.collection.mutable.{ LinkedHashMap }

case class SendBackgroundMessageEvent(code: Int, detail: Option[String])
case class RPCResultEvent(value: WireFormat, callId: Int)
case class RPCErrorEvent(code: Int, detail: Option[String], callId: Int)
case class RPCRequestEvent(req: Any, callId: Int)

case class ClearAllNotesEvent(lang: scala.Symbol)
case class NewNotesEvent(lang: scala.Symbol, notelist: NoteList)

case class AnalyzerReadyEvent()
case class AnalyzerShutdownEvent()
case class IndexerReadyEvent()

case class ReloadFileReq(file: File)
case class ReloadAllReq()
case class RemoveFileReq(file: File)
case class ScopeCompletionReq(file: File, point: Int, prefix: String, constructor: Boolean)
case class TypeCompletionReq(file: File, point: Int, prefix: String)
case class ImportSuggestionsReq(file: File, point: Int, names: List[String], maxResults: Int)
case class PublicSymbolSearchReq(names: List[String], maxResults: Int)
case class UsesOfSymAtPointReq(file: File, point: Int)
case class PackageMemberCompletionReq(path: String, prefix: String)
case class SymbolAtPointReq(file: File, point: Int)
case class InspectTypeReq(file: File, point: Int)
case class InspectTypeByIdReq(id: Int)
case class InspectPackageByPathReq(path: String)
case class TypeByIdReq(id: Int)
case class TypeByNameReq(name: String)
case class TypeByNameAtPointReq(name: String, file: File, point: Int)
case class CallCompletionReq(id: Int)
case class TypeAtPointReq(file: File, point: Int)

case class AddUndo(summary: String, changes: List[Change])
case class Undo(id: Int, summary: String, changes: Iterable[Change])
case class UndoResult(id: Int, touched: Iterable[File])

class Project(val protocol: Protocol) extends Actor with RPCTarget {

  protocol.setRPCTarget(this)

  // Note: would like to use Option[ProjectConfig] here but causes
  // prez-compiler to kerplode :\
  protected var config: ProjectConfig = ProjectConfig.nullConfig

  protected var analyzer: Actor = actor {}
  protected var builder: Option[Actor] = None
  protected var debugInfo: Option[ProjectDebugInfo] = None

  private var undoCounter = 0
  private val undos: LinkedHashMap[Int, Undo] = new LinkedHashMap[Int, Undo]

  def act() {
    println("Project waiting for init...")
    loop {
      try {
        receive {
          case SendBackgroundMessageEvent(code: Int, detail: Option[String]) => {
            protocol.sendBackgroundMessage(code, detail)
          }
          case IncomingMessageEvent(msg: WireFormat) => {
            protocol.handleIncomingMessage(msg)
          }
          case AnalyzerReadyEvent() => {
            protocol.sendCompilerReady
          }
          case FullTypeCheckCompleteEvent() => {
            protocol.sendFullTypeCheckComplete
          }
          case msg: IndexerReadyEvent => {
            protocol.sendIndexerReady
          }
          case NewNotesEvent(lang, notes:NoteList) => {
            protocol.sendNotes(lang, notes)
          }
          case ClearAllNotesEvent(lang) => {
            protocol.sendClearAllNotes(lang)
          }
          case AddUndo(sum, changes) => {
            addUndo(sum, changes)
          }
          case RPCResultEvent(value, callId) => {
            protocol.sendRPCReturn(value, callId)
          }
          case RPCErrorEvent(code, detail, callId) => {
            protocol.sendRPCError(code, detail, callId)
          }
        }
      } catch {
        case e: Exception => {
          println("Error at Project message loop: " + e + " :\n" + e.getStackTraceString)
        }
      }
    }
  }

  protected def addUndo(sum: String, changes: Iterable[Change]) {
    undoCounter += 1
    undos(undoCounter) = Undo(undoCounter, sum, changes)
  }

  protected def peekUndo(): Either[String, Undo] = {
    undos.lastOption match {
      case Some(u) => Right(u._2)
      case _ => Left("No such undo.")
    }
  }

  protected def execUndo(undoId: Int): Either[String, UndoResult] = {
    undos.get(undoId) match {
      case Some(u) => {
        undos.remove(u.id)
        FileUtils.writeChanges(u.changes) match {
          case Right(touched) => Right(UndoResult(undoId, touched))
          case Left(e) => Left(e.getMessage())
        }
      }
      case _ => Left("No such undo.")
    }
  }

  protected def initProject(conf: ProjectConfig) {
    config = conf
    restartCompiler
    shutdownBuilder
    undos.clear
    undoCounter = 0
  }

  protected def restartCompiler() {
    analyzer ! AnalyzerShutdownEvent()
    analyzer = new Analyzer(this, protocol, config)
    analyzer.start
  }

  protected def getOrStartBuilder(): Actor = {
    builder match {
      case Some(b) => b
      case None =>
        {
        val b = new IncrementalBuilder(this, protocol, config)
        builder = Some(b)
        b.start
        b
      }
    }
  }

  protected def shutdownBuilder() {
    for (b <- builder) {
      b ! BuilderShutdownEvent
    }
    builder = None
  }

  protected def shutdownServer() {
    System.out.println("Server is exiting...")
    System.out.flush()
    System.exit(0)
  }

}

