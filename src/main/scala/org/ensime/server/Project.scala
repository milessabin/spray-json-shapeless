package org.ensime.server

import java.io.File
import akka.actor.{ Actor, ActorRef, Props, ActorSystem, Cancellable }
import org.apache.commons.vfs2.FileObject
import org.ensime.config._
import org.ensime.indexer._
import org.ensime.model._
import org.ensime.protocol._
import org.ensime.util._
import org.slf4j.LoggerFactory
import scala.collection.mutable
import scala.concurrent.duration._

case class RPCResultEvent(value: WireFormat, callId: Int)
case class RPCErrorEvent(code: Int, detail: String, callId: Int)
case class RPCRequestEvent(req: Any, callId: Int)

case class AsyncEvent(evt: SwankEvent)

case object AnalyzerShutdownEvent
case object ReloadExistingFilesEvent
case object AskReTypecheck

case class ReloadFilesReq(files: List[SourceFileInfo])
case object ReloadAllReq
case object UnloadAllReq
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
case class InspectTypeByNameReq(name: String)
case class InspectPackageByPathReq(path: String)
case class TypeByIdReq(id: Int)
case class MemberByNameReq(typeFullName: String, memberName: String, memberIsType: Boolean)
case class TypeByNameReq(name: String)
case class TypeByNameAtPointReq(name: String, file: File, range: OffsetRange)
case class CallCompletionReq(id: Int)
case class TypeAtPointReq(file: File, range: OffsetRange)
case class SymbolDesignationsReq(file: File, start: Int, end: Int, tpes: List[Symbol])

case class AddUndo(summary: String, changes: List[FileEdit])
case class Undo(id: Int, summary: String, changes: List[FileEdit])
case class UndoResult(id: Int, touched: Iterable[File])

case object ClientConnectedEvent

class Project(
    val config: EnsimeConfig,
    val protocol: Protocol,
    actorSystem: ActorSystem) extends ProjectRPCTarget {
  val log = LoggerFactory.getLogger(this.getClass)

  protected val actor = actorSystem.actorOf(Props(new ProjectActor()), "project")

  def !(msg: AnyRef) {
    actor ! msg
  }

  protocol.setRPCTarget(this)

  protected var analyzer: Option[ActorRef] = None

  private val resolver = new SourceResolver(config)
  // TODO: add PresCompiler to the source watcher
  private val sourceWatcher = new SourceWatcher(config, resolver :: Nil)
  private val search = new SearchService(config, resolver)
  private val reTypecheck = new ClassfileListener {
    def reTypeCheck(): Unit = actor ! AskReTypecheck
    def classfileAdded(f: FileObject): Unit = reTypeCheck()
    def classfileChanged(f: FileObject): Unit = reTypeCheck()
    def classfileRemoved(f: FileObject): Unit = reTypeCheck()
  }
  private val classfileWatcher = new ClassfileWatcher(config, search :: reTypecheck :: Nil)

  import concurrent.ExecutionContext.Implicits.global
  search.refresh().onSuccess {
    case (deletes, inserts) =>
      actor ! AsyncEvent(IndexerReadyEvent)
      log.debug(s"indexed $inserts and removed $deletes")
  }

  protected val indexer: ActorRef = actorSystem.actorOf(Props(
    new Indexer(config, search, this, protocol.conversions)), "indexer")

  protected var debugger: Option[ActorRef] = None

  def getAnalyzer: ActorRef = {
    analyzer.getOrElse(throw new RuntimeException("Analyzer unavailable."))
  }
  def getIndexer: ActorRef = indexer

  private var undoCounter = 0
  private val undos: mutable.LinkedHashMap[Int, Undo] = new mutable.LinkedHashMap[Int, Undo]

  def sendRPCError(code: Int, detail: String, callId: Int) {
    actor ! RPCErrorEvent(code, detail, callId)
  }

  def bgMessage(msg: String) {
    actor ! AsyncEvent(SendBackgroundMessageEvent(ProtocolConst.MsgMisc, Some(msg)))
  }

  class ProjectActor extends Actor {
    case object Retypecheck

    val typecheckDelay = 1000 millis
    val typecheckCooldown = 5000 millis
    private var tick: Option[Cancellable] = None

    private var earliestRetypecheck = Deadline.now

    override def postStop(): Unit = {
      tick.foreach(_.cancel())
    }

    override def receive = waiting orElse connected

    // buffer until the client connects
    private var asyncs: List[AsyncEvent] = Nil

    private val waiting: Receive = {
      case ClientConnectedEvent =>
        asyncs foreach {
          case AsyncEvent(value) =>
            protocol.sendEvent(value)
        }
        asyncs = Nil
        context.become(connected, true)

      case e: AsyncEvent =>
        asyncs ::= e
    }

    private val connected: Receive = {
      case Retypecheck =>
        log.warn("Re-typecheck needed")
        analyzer.foreach(_ ! ReloadExistingFilesEvent)
        earliestRetypecheck = typecheckCooldown.fromNow
      case AskReTypecheck =>
        tick.foreach(_.cancel())
        val timeToTypecheck = earliestRetypecheck.timeLeft max typecheckDelay
        tick = Some(context.system.scheduler.scheduleOnce(timeToTypecheck, self, Retypecheck))

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
    startCompiler()
    shutdownDebugger()
    undos.clear()
    undoCounter = 0
  }

  protected def startCompiler() {
    val newAnalyzer = actorSystem.actorOf(Props(
      new Analyzer(this, indexer, search, protocol.conversions, config)
    ), "analyzer")
    analyzer = Some(newAnalyzer)
  }

  protected def acquireDebugger(): ActorRef = {
    (debugger, indexer) match {
      case (Some(b), _) => b
      case (None, indexer) =>
        val b = actorSystem.actorOf(Props(new DebugManager(this, indexer, protocol.conversions, config)))
        debugger = Some(b)
        b
    }
  }

  protected def shutdownDebugger() {
    debugger.foreach(_ ! DebuggerShutdownEvent)
    debugger = None
  }

  protected def shutdownServer() {
    log.info("Server is exiting...")
    System.exit(0)
  }
}

