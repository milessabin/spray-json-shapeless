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
case class RPCErrorEvent(code: Int, detail: String, callId: Int)
case class RPCRequestEvent(req: Any, callId: Int)

case class AsyncEvent(evt: SwankEvent)

case object AnalyzerShutdownEvent

case class ReloadFilesReq(files: List[SourceFileInfo])
case object ReloadAllReq
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

case object ClientConnectedEvent

class Project(projectConfig: ProjectConfig,
    cacheDir: File,
    val protocol: Protocol,
    actorSystem: ActorSystem) extends ProjectRPCTarget {
  val log = LoggerFactory.getLogger(this.getClass)

  protected val actor = actorSystem.actorOf(Props(new ProjectActor()), "project")

  def !(msg: AnyRef) {
    actor ! msg
  }

  protocol.setRPCTarget(this)

  protected var config: ProjectConfig = projectConfig

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

  def sendRPCError(code: Int, detail: String, callId: Int) {
    actor ! RPCErrorEvent(code, detail, callId)
  }

  def bgMessage(msg: String) {
    actor ! AsyncEvent(SendBackgroundMessageEvent(ProtocolConst.MsgMisc, Some(msg)))
  }

  class ProjectActor extends Actor {
    var indexerReady = false

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
        case IndexerReadyEvent =>
          log.info("Project notified that indexer is ready")
          self ! AsyncEvent(IndexerReadyEvent)
        case ClientConnectedEvent =>
          log.info("ClientConnectedEvent" + indexerReady)
          // if the indexer had finished initialing generate a fake IndexerReadyEvent to notify emacs we are ready.
          if (indexerReady)
            protocol.sendEvent(IndexerReadyEvent)
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
            analyzer.foreach(ea => ea ! ReloadFilesReq(touched.toList.map { SourceFileInfo(_) }))
            Right(UndoResult(undoId, touched))
          case Left(e) => Left(e.getMessage)
        }
      case _ => Left("No such undo.")
    }
  }

  def initProject() {
    startIndexer()
    startCompiler()
    shutdownDebugger()
    undos.clear()
    undoCounter = 0
  }

  protected def startIndexer() {
    indexer.foreach(_ ! IndexerShutdownReq)
    indexer = None
    val newIndexer = actorSystem.actorOf(Props(new Indexer(this, this.cacheDir, protocol.conversions, config)), "indexer")
    log.info("Initialising Indexer...")

    if (!config.disableIndexOnStartup) {
      newIndexer ! RebuildStaticIndexReq
    }
    indexer = Some(newIndexer)
  }

  protected def startCompiler() {
    indexer match {
      case Some(indexerVal) =>
        val newAnalyzer = actorSystem.actorOf(Props(new Analyzer(this, indexerVal, protocol.conversions, config)), "analyzer")
        analyzer = Some(newAnalyzer)
      case None =>
        throw new RuntimeException("Indexer must be started before analyzer.")
    }
  }

  protected def acquireDebugger(): ActorRef = {
    ((debugger, indexer) match {
      case (Some(b), _) => Some(b)
      case (None, Some(indexerVal)) =>
        val b = actorSystem.actorOf(Props(new DebugManager(this, indexerVal, protocol.conversions, config)))
        debugger = Some(b)
        Some(b)
      case _ =>
        None
    }).getOrElse(throw new RuntimeException("Indexer must be started before debug manager."))
  }

  protected def shutdownDebugger() {
    debugger.foreach(_ ! DebuggerShutdownEvent)
    debugger = None
  }

  protected def shutdownServer() {
    System.out.println("Server is exiting...")
    System.out.flush()

    indexer.foreach(_ ! IndexerShutdownReq)
    analyzer.foreach(_ ! AnalyzerShutdownEvent)

    Thread.sleep(5000)
    System.exit(0)
  }
}

