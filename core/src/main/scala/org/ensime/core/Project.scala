package org.ensime.core

import akka.actor._
import akka.event.LoggingReceive
import akka.event.LoggingReceive
import org.apache.commons.vfs2.FileObject

import org.ensime.api._

import org.ensime.config._
import org.ensime.indexer._
import org.ensime.model._
import org.ensime.util._
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ Future, Promise }
import scala.util.Try

case class EnsimeServerError(description: String)

/**
 * The Project actor simply forwards messages coming from the user to
 * the respective subcomponent.
 *
 * @param target for async messages
 */
class Project(
    target: ActorRef,
    implicit val config: EnsimeConfig
) extends Actor with ActorLogging with Stash {
  import context.system
  import context.dispatcher

  /* The main components of the ENSIME server */
  private var analyzer: ActorRef = _
  private var debugger: ActorRef = _

  // TODO consolidate search/indexer
  private var indexer: ActorRef = _

  // TODO: use state transitions to manage this state
  private var indexerReady: Boolean = _
  private var analyserReady: Boolean = _

  // vfs, resolver, search and watchers are considered "reliable" (hah!)
  // TODO: Actor-ise as many of these vals as possible
  private implicit val vfs: EnsimeVFS = EnsimeVFS()
  private val resolver = new SourceResolver(config)
  private val searchService = new SearchService(config, resolver)
  searchService.refresh().onSuccess {
    case (deletes, inserts) =>
      target ! IndexerReadyEvent
      log.debug(s"indexed $inserts and removed $deletes")
  }(context.dispatcher)

  private val sourceWatcher = new SourceWatcher(config, resolver :: Nil)
  private val reTypecheck = new ClassfileListener {
    def reTypeCheck(): Unit = self ! AskReTypecheck
    def classfileAdded(f: FileObject): Unit = reTypeCheck()
    def classfileChanged(f: FileObject): Unit = reTypeCheck()
    def classfileRemoved(f: FileObject): Unit = reTypeCheck()
  }
  private val classfileWatcher = new ClassfileWatcher(config, searchService :: reTypecheck :: Nil)

  override def preStart(): Unit = {
    indexer = context.actorOf(Props(new Indexer(config, searchService)), "indexer")
    analyzer = context.actorOf(Props(new Analyzer(target, indexer, searchService, config)), "analyzer")
    debugger = context.actorOf(Props(new DebugManager(target, config)), "debugging")
  }

  override def postStop(): Unit = {
    // make sure the "reliable" dependencies are cleaned up
    Try(classfileWatcher.shutdown())
    Try(sourceWatcher.shutdown())
    Try(searchService.shutdown())
    Try(vfs.close())
  }

  // debounces ReloadExistingFilesEvent
  private var rechecking: Cancellable = _

  def receive: Receive =
    filesChanging orElse LoggingReceive { respondingToQueries }

  def filesChanging: Receive = {
    case AskReTypecheck =>
      Option(rechecking).foreach(_.cancel())
      rechecking = system.scheduler.scheduleOnce(
        5 seconds, analyzer, ReloadExistingFilesEvent
      )
  }

  def respondingToQueries: Receive = {
    case ConnectionInfoReq => sender() ! ConnectionInfo()

    case m: RpcAnalyserRequest => analyzer forward m
    case m: RpcDebuggerRequest => debugger forward m
    case m: RpcSearchRequest => indexer forward m
  }

  /*
  Funky non-standard cases:

   - SymbolByNameReq (option)

   - PrepareRefactorReq (complex)
   - ExecRefactorReq (either)

   - DebugLocateNameReq (option)
   - DebugValueReq (option)
   - DebugToStringReq (option)

   */

}
object Project {
  def apply(target: ActorRef)(implicit config: EnsimeConfig): Props =
    Props(classOf[Project], target, config)
}
