package org.ensime.core

import java.io.File
import akka.actor.{ Actor, ActorRef, Props, ActorSystem, Cancellable }
import org.apache.commons.vfs2.FileObject
import org.ensime.config._
import org.ensime.indexer._
import org.ensime.model._
import org.ensime.util._
import org.slf4j.LoggerFactory
import scala.collection.mutable
import scala.concurrent.duration._

case class RPCError(code: Int, detail: String) extends RuntimeException()
case class AsyncEvent(evt: EnsimeEvent)

case object AnalyzerShutdownEvent
case object ReloadExistingFilesEvent
case object AskReTypecheck

case object VoidResponse

trait RPCRequest

case class ReloadFilesReq(files: List[SourceFileInfo], async: Boolean) extends RPCRequest
case object ReloadAllReq extends RPCRequest
case object UnloadAllReq extends RPCRequest
case class PatchSourceReq(file: File, edits: List[PatchOp]) extends RPCRequest
case class RemoveFileReq(file: File) extends RPCRequest
case class CompletionsReq(fileInfo: SourceFileInfo, point: Int, maxResults: Int, caseSens: Boolean) extends RPCRequest
case class TypeCompletionsReq(prefix: String, maxResults: Int) extends RPCRequest
case class ImportSuggestionsReq(file: File, point: Int, names: List[String], maxResults: Int) extends RPCRequest
case class PublicSymbolSearchReq(names: List[String], maxResults: Int) extends RPCRequest
case class UsesOfSymAtPointReq(file: File, point: Int) extends RPCRequest
case class PackageMemberCompletionReq(path: String, prefix: String) extends RPCRequest
case class SymbolAtPointReq(file: File, point: Int) extends RPCRequest
case class InspectTypeReq(file: File, range: OffsetRange) extends RPCRequest
case class InspectTypeByIdReq(id: Int) extends RPCRequest
case class InspectTypeByNameReq(name: String) extends RPCRequest
case class InspectPackageByPathReq(path: String) extends RPCRequest
case class TypeByIdReq(id: Int) extends RPCRequest
case class MemberByNameReq(typeFullName: String, memberName: String, memberIsType: Boolean) extends RPCRequest
case class TypeByNameReq(name: String) extends RPCRequest
case class TypeByNameAtPointReq(name: String, file: File, range: OffsetRange) extends RPCRequest
case class CallCompletionReq(id: Int) extends RPCRequest
case class TypeAtPointReq(file: File, range: OffsetRange) extends RPCRequest
case class SymbolDesignationsReq(file: File, start: Int, end: Int, tpes: Set[SourceSymbol]) extends RPCRequest
case class ExecUndoReq(undo: Undo) extends RPCRequest
case class FormatFilesReq(filenames: List[String]) extends RPCRequest
case class FormatFileReq(fileInfo: SourceFileInfo) extends RPCRequest
case class ExpandSelectionReq(filename: String, start: Int, stop: Int) extends RPCRequest

case class SubscribeAsync(handler: EnsimeEvent => Unit) extends RPCRequest

case object ClientReadyEvent

class Project(
    val config: EnsimeConfig,
    actorSystem: ActorSystem) extends ProjectEnsimeApiImpl {
  val log = LoggerFactory.getLogger(this.getClass)

  protected val actor = actorSystem.actorOf(Props(new ProjectActor()), "project")

  def !(msg: AnyRef): Unit = {
    actor ! msg
  }

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
    new Indexer(config, search, this)), "indexer")

  protected var debugger: Option[ActorRef] = None

  def getAnalyzer: ActorRef = {
    analyzer.getOrElse(throw new RuntimeException("Analyzer unavailable."))
  }

  private var undoCounter = 0
  private val undos: mutable.LinkedHashMap[Int, Undo] = new mutable.LinkedHashMap[Int, Undo]

  class ProjectActor extends Actor {
    case object Retypecheck

    val typecheckDelay = 1000.millis
    val typecheckCooldown = 5000.millis
    private var tick: Option[Cancellable] = None

    private var earliestRetypecheck = Deadline.now

    override def postStop(): Unit = {
      tick.foreach(_.cancel())
    }

    // buffer events until the first client connects
    private var asyncEvents = Vector[EnsimeEvent]()
    private var asyncListeners: List[EnsimeEvent => Unit] = Nil

    override def receive: Receive = waiting orElse ready

    private val ready: Receive = {
      case Retypecheck =>
        log.warn("Re-typecheck needed")
        analyzer.foreach(_ ! ReloadExistingFilesEvent)
        earliestRetypecheck = typecheckCooldown.fromNow
      case AskReTypecheck =>
        tick.foreach(_.cancel())
        val timeToTypecheck = earliestRetypecheck.timeLeft max typecheckDelay
        tick = Some(context.system.scheduler.scheduleOnce(timeToTypecheck, self, Retypecheck))

      case AddUndo(sum, changes) =>
        addUndo(sum, changes)

      case AsyncEvent(event) =>
        asyncListeners foreach { l =>
          l(event)
        }
      case SubscribeAsync(handler) =>
        asyncListeners ::= handler
        sender ! false
    }

    private val waiting: Receive = {
      case ClientReadyEvent =>
      case SubscribeAsync(handler) =>
        asyncListeners ::= handler
        asyncEvents.foreach { event => handler(event) }
        asyncEvents = Vector.empty
        context.become(ready, discardOld = true)
        sender ! true
      case AsyncEvent(event) =>
        asyncEvents :+= event
    }
  }

  protected def addUndo(sum: String, changes: Iterable[FileEdit]): Unit = {
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
        callRPC[Either[String, UndoResult]](getAnalyzer, ExecUndoReq(u))
      case _ => Left("No such undo.")
    }
  }

  def initProject(): Unit = {
    startCompiler()
    shutdownDebugger()
    undos.clear()
    undoCounter = 0
  }

  protected def startCompiler(): Unit = {
    val newAnalyzer = actorSystem.actorOf(Props(
      new Analyzer(actor, indexer, search, config)), "analyzer")
    analyzer = Some(newAnalyzer)
  }

  protected def acquireDebugger: ActorRef = {
    debugger match {
      case Some(d) => d
      case None =>
        val d = actorSystem.actorOf(Props(new DebugManager(actor, indexer, config)))
        debugger = Some(d)
        d
    }
  }

  protected def shutdownDebugger(): Unit = {
    debugger.foreach(_ ! DebuggerShutdownEvent)
    debugger = None
  }

  protected def shutdownServer(): Unit = {
    val t = new Thread() {
      override def run(): Unit = {
        log.info("Server is exiting...")
        Thread.sleep(1000)
        log.info("Shutting down actor system...")
        actorSystem.shutdown()
        Thread.sleep(1000)
        log.info("Forcing exit...")
        Thread.sleep(200)
        System.exit(0)
      }
    }
    t.start()
  }
}

