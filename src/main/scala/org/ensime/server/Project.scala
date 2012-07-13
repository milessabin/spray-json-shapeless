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

package org.ensime.server

import java.io.File
import org.ensime.config.ProjectConfig

import org.ensime.model.PatchOp
import org.ensime.protocol._
import org.ensime.util._
import scala.actors._
import scala.actors.Actor._
import scala.tools.nsc.{ Settings }
import scala.collection.mutable.{ LinkedHashMap }

case class RPCResultEvent(value: WireFormat, callId: Int)
case class RPCErrorEvent(code: Int, detail: Option[String], callId: Int)
case class RPCRequestEvent(req: Any, callId: Int)
case class AsyncEvent(evt: WireFormat)

case class ClearAllNotesEvent(lang: scala.Symbol)
case class NewNotesEvent(lang: scala.Symbol, notelist: NoteList)
case class SendBackgroundMessageEvent(code: Int, detail: Option[String])
case class AnalyzerReadyEvent()
case class AnalyzerShutdownEvent()
case class IndexerReadyEvent()

case class ReloadFileReq(file: File)
case class ReloadAllReq()
case class PatchSourceReq(file: File, edits: List[PatchOp])
case class RemoveFileReq(file: File)
case class CompletionsReq(
  file: File, point: Int, maxResults: Int, caseSens: Boolean, reload: Boolean)
case class ImportSuggestionsReq(
  file: File, point: Int, names: List[String], maxResults: Int)
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
case class SymbolDesignationsReq(
  file: File, start: Int, end: Int, tpes: List[Symbol])

case class AddUndo(summary: String, changes: List[FileEdit])
case class Undo(id: Int, summary: String, changes: List[FileEdit])
case class UndoResult(id: Int, touched: Iterable[File])

class Project(val protocol: Protocol) extends Actor with RPCTarget {

  protocol.setRPCTarget(this)

  // TODO(aemoncannon) would like to use Option[ProjectConfig] here but causes
  // prez-compiler to kerplode :\
  protected var config: ProjectConfig = ProjectConfig.nullConfig

  protected var analyzer: Actor = actor {}
  protected var builder: Option[Actor] = None
  protected var debugger: Option[Actor] = None

  private var undoCounter = 0
  private val undos: LinkedHashMap[Int, Undo] = new LinkedHashMap[Int, Undo]

  def sendRPCError(code: Int, detail: Option[String], callId: Int) {
    this ! RPCErrorEvent(code, detail, callId)
  }
  def sendRPCError(detail: String, callId: Int) {
    sendRPCError(ProtocolConst.ErrExceptionInRPC, Some(detail), callId)
  }

  def bgMessage(msg: String) {
    this ! AsyncEvent(protocol.toWF(SendBackgroundMessageEvent(
      ProtocolConst.MsgMisc,
      Some(msg))))
  }

  def act() {
    println("Project waiting for init...")
    loop {
      try {
        receive {
          case IncomingMessageEvent(msg: WireFormat) => {
            protocol.handleIncomingMessage(msg)
          }
          case AddUndo(sum, changes) => {
            addUndo(sum, changes)
          }
          case RPCResultEvent(value, callId) => {
            protocol.sendRPCReturn(value, callId)
          }
          case AsyncEvent(value) => {
            protocol.sendEvent(value)
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

  protected def addUndo(sum: String, changes: Iterable[FileEdit]) {
    undoCounter += 1
    undos(undoCounter) = Undo(undoCounter, sum, changes.toList)
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
    shutdownDebugger
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

  protected def getOrStartDebugger(): Actor = {
    debugger match {
      case Some(b) => b
      case None =>
        {
          val b = new DebugManager(this, protocol, config)
          debugger = Some(b)
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
  protected def shutdownDebugger() {
    for (d <- debugger) {
      d ! DebuggerShutdownEvent
    }
    debugger = None
  }

  protected def shutdownServer() {
    System.out.println("Server is exiting...")
    System.out.flush()
    System.exit(0)
  }

}

