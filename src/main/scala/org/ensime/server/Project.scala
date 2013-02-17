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
import org.ensime.model.OffsetRange
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

case class ReloadFilesReq(files: List[File])
case class ReloadAllReq()
case class PatchSourceReq(file: File, edits: List[PatchOp])
case class RemoveFileReq(file: File)
case class CompletionsReq(
  file: File, point: Int, maxResults: Int, caseSens: Boolean, reload: Boolean)
case class ImportSuggestionsReq(
  file: File, point: Int, names: List[String], maxResults: Int)
case class PublicSymbolSearchReq(names: List[String], maxResults: Int)
case class MethodBytecodeReq(sourceName: String, line: Int)
case class UsesOfSymAtPointReq(file: File, point: Int)
case class PackageMemberCompletionReq(path: String, prefix: String)
case class SymbolAtPointReq(file: File, point: Int)
case class InspectTypeReq(file: File, range: OffsetRange)
case class InspectTypeByIdReq(id: Int)
case class InspectPackageByPathReq(path: String)
case class TypeByIdReq(id: Int)
case class TypeByNameReq(name: String)
case class TypeByNameAtPointReq(name: String, file: File, range: OffsetRange)
case class CallCompletionReq(id: Int)
case class TypeAtPointReq(file: File, range: OffsetRange)
case class SymbolDesignationsReq(
  file: File, start: Int, end: Int, tpes: List[Symbol])

case class AddUndo(summary: String, changes: List[FileEdit])
case class Undo(id: Int, summary: String, changes: List[FileEdit])
case class UndoResult(id: Int, touched: Iterable[File])

class Project(val protocol: Protocol) extends Actor with RPCTarget {

  protocol.setRPCTarget(this)

  protected var config: ProjectConfig = ProjectConfig.nullConfig

  protected var analyzer: Option[Actor] = None
  protected var indexer: Option[Actor] = None
  protected var builder: Option[Actor] = None
  protected var debugger: Option[Actor] = None

  def getAnalyzer: Actor = {
    analyzer.getOrElse(throw new RuntimeException(
	"Analyzer unavailable."))
  }
  def getIndexer: Actor = {
    indexer.getOrElse(throw new RuntimeException(
	"Indexer unavailable."))
  }


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
          case Right(touched) => {
	    for(ea <- analyzer) {
	      ea ! ReloadFilesReq(touched.toList)
	    }
	    Right(UndoResult(undoId, touched))
	  }
          case Left(e) => Left(e.getMessage())
        }
      }
      case _ => Left("No such undo.")
    }
  }

  protected def initProject(conf: ProjectConfig) {
    config = conf
    restartIndexer
    restartCompiler
    shutdownBuilder
    shutdownDebugger
    undos.clear
    undoCounter = 0
  }

  protected def restartIndexer() {
    for(ea <- indexer) {
      ea ! IndexerShutdownReq()
    }
    val newIndexer = new Indexer(this, protocol, config)
    println("Initing Indexer...")
    newIndexer.start
    if (!config.disableIndexOnStartup) {
      newIndexer ! RebuildStaticIndexReq()
    }
    indexer = Some(newIndexer)
  }

  protected def restartCompiler() {
    for(ea <- analyzer) {
      ea ! AnalyzerShutdownEvent()
    }
    indexer match{
      case Some(indexer) => {
	val newAnalyzer = new Analyzer(this, indexer, protocol, config)
	newAnalyzer.start
	analyzer = Some(newAnalyzer)
      }
      case None => {
	throw new RuntimeException("Indexer must be started before analyzer.")
      }
    }
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
    ((debugger, indexer) match {
      case (Some(b), _) => Some(b)
      case (None, Some(indexer)) => {
        val b = new DebugManager(this, indexer, protocol, config)
        debugger = Some(b)
        b.start
        Some(b)
      }
      case _ => None
    }).getOrElse(throw new RuntimeException(
      "Indexer must be started before debug manager."
    ))
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

