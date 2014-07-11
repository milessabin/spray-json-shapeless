package org.ensime.server

import java.io.File
import akka.actor.{ Actor, ActorRef, Props, ActorSystem }
import org.ensime.config.ProjectConfig

import org.ensime.model.PatchOp
import org.ensime.model.SourceFileInfo
import org.ensime.model.OffsetRange
import org.ensime.protocol._
import org.ensime.util._
import org.slf4j.LoggerFactory
import scala.collection.mutable

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

case class ReloadFilesReq(files: List[SourceFileInfo])
case class ReloadAllReq()
case class PatchSourceReq(file: File, edits: List[PatchOp])
case class RemoveFileReq(file: File)
case class CompletionsReq(file: File, point: Int, maxResults: Int, caseSens: Boolean, reload: Boolean)
case class ImportSuggestionsReq(file: File, point: Int, names: List[String], maxResults: Int)
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
case class SymbolDesignationsReq(file: File, start: Int, end: Int, tpes: List[Symbol])

case class AddUndo(summary: String, changes: List[FileEdit])
case class Undo(id: Int, summary: String, changes: List[FileEdit])
case class UndoResult(id: Int, touched: Iterable[File])

class Project(val protocol: Protocol, actorSystem: ActorSystem) extends ProjectRPCTarget {
  val log = LoggerFactory.getLogger(this.getClass)

  private val actor = actorSystem.actorOf(Props(new ProjectActor()), "project")

  def !(msg: AnyRef) {
    actor ! msg
  }
  // TODO This is lethal - Project is both an actor and a threaded callback target
  protocol.setRPCTarget(this)

  protected var config: ProjectConfig = ProjectConfig.nullConfig

  protected var analyzer: Option[ActorRef] = None
  protected var indexer: Option[ActorRef] = None
  protected var debugger: Option[ActorRef] = None

  def getAnalyzer: ActorRef = {
    analyzer.getOrElse(throw new RuntimeException("Analyzer unavailable."))
  }
  def getIndexer: ActorRef = {
    indexer.getOrElse(throw new RuntimeException("Indexer unavailable."))
  }

  private var undoCounter = 0
  private val undos: mutable.LinkedHashMap[Int, Undo] = new mutable.LinkedHashMap[Int, Undo]

  def sendRPCError(code: Int, detail: Option[String], callId: Int) {
    actor ! RPCErrorEvent(code, detail, callId)
  }
  def sendRPCError(detail: String, callId: Int) {
    sendRPCError(ProtocolConst.ErrExceptionInRPC, Some(detail), callId)
  }

  def bgMessage(msg: String) {
    actor ! AsyncEvent(protocol.conversions.toWF(SendBackgroundMessageEvent(
      ProtocolConst.MsgMisc,
      Some(msg))))
  }

  class ProjectActor extends Actor {
    override def receive = {
      case x: Any =>
        try {
          process(x)
        } catch {
          case e: Exception =>
            log.error("Error at Project message loop: ", e)
        }

    }

    log.info("Project waiting for init...")

    def process(msg: Any): Unit = {
      msg match {
        case IncomingMessageEvent(msg: WireFormat) =>
          protocol.handleIncomingMessage(msg)
        case AddUndo(sum, changes) =>
          addUndo(sum, changes)
        case RPCResultEvent(value, callId) =>
          protocol.sendRPCReturn(value, callId)
        case AsyncEvent(value) =>
          protocol.sendEvent(value)
        case RPCErrorEvent(code, detail, callId) =>
          protocol.sendRPCError(code, detail, callId)
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
      case Some(u) =>
        undos.remove(u.id)
        FileUtils.writeChanges(u.changes) match {
          case Right(touched) =>
            for (ea <- analyzer) {
              ea ! ReloadFilesReq(touched.toList.map { SourceFileInfo(_) })
            }
            Right(UndoResult(undoId, touched))
          case Left(e) => Left(e.getMessage)
        }
      case _ => Left("No such undo.")
    }
  }

  protected def initProject(conf: ProjectConfig) {
    config = conf
    restartIndexer()
    restartCompiler()
    shutdownDebugger()
    undos.clear()
    undoCounter = 0
  }

  protected def restartIndexer() {
    for (ea <- indexer) {
      ea ! IndexerShutdownReq()
    }
    val newIndexer = actorSystem.actorOf(Props(new Indexer(this, protocol.conversions, config)), "indexer")
    log.info("Initing Indexer...")
    if (!config.disableIndexOnStartup) {
      newIndexer ! RebuildStaticIndexReq()
    }
    indexer = Some(newIndexer)
  }

  protected def restartCompiler() {
    for (ea <- analyzer) {
      ea ! AnalyzerShutdownEvent()
    }
    indexer match {
      case Some(indexer) =>
        val newAnalyzer = actorSystem.actorOf(Props(new Analyzer(this, indexer, protocol.conversions, config)), "analyzer")
        analyzer = Some(newAnalyzer)
      case None =>
        throw new RuntimeException("Indexer must be started before analyzer.")
    }
  }

  protected def getOrStartDebugger(): ActorRef = {
    ((debugger, indexer) match {
      case (Some(b), _) => Some(b)
      case (None, Some(indexer)) =>
        val b = actorSystem.actorOf(Props(new DebugManager(this, indexer, protocol.conversions, config)))
        debugger = Some(b)
        Some(b)
      case _ => None
    }).getOrElse(throw new RuntimeException("Indexer must be started before debug manager."))
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

