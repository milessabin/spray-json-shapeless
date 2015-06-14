package org.ensime.core

import akka.event.LoggingReceive
import java.io.File
import java.nio.charset.Charset

import akka.actor.{ Actor, ActorLogging, ActorRef }

import org.ensime.api._
import org.ensime.config._
import org.ensime.indexer.{ EnsimeVFS, SearchService }
import org.ensime.model._
import org.ensime.server.protocol._
import org.ensime.util._
import org.slf4j.LoggerFactory

import scala.concurrent.Future
import scala.reflect.internal.util.{ OffsetPosition, RangePosition, SourceFile }
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global

import ProtocolConst._

import pimpathon.file._

case class CompilerFatalError(e: Throwable)

class Analyzer(
    val project: ActorRef,
    val indexer: ActorRef,
    search: SearchService,
    val config: EnsimeConfig
)(
    implicit
    vfs: EnsimeVFS
) extends Actor with ActorLogging with RefactoringHandler {

  private val presCompLog = LoggerFactory.getLogger(classOf[Global])
  private val settings = new Settings(presCompLog.error)
  settings.YpresentationDebug.value = presCompLog.isTraceEnabled
  settings.YpresentationVerbose.value = presCompLog.isDebugEnabled
  settings.verbose.value = presCompLog.isDebugEnabled
  settings.usejavacp.value = false

  config.allJars.find(_.getName.contains("scala-library")) match {
    case Some(scalaLib) => settings.bootclasspath.value = scalaLib.getAbsolutePath
    case None => log.warning("scala-library.jar was not present")
  }
  settings.classpath.value = config.compileClasspath.mkString(File.pathSeparator)

  settings.processArguments(config.compilerArgs, processAll = false)

  log.info("Presentation Compiler settings:\n" + settings)

  private val reportHandler = new ReportHandler {
    override def messageUser(str: String): Unit = {
      project ! AsyncEvent(SendBackgroundMessageEvent(MsgCompilerUnexpectedError, Some(str)))
    }
    override def clearAllScalaNotes(): Unit = {
      project ! AsyncEvent(ClearAllScalaNotesEvent)
    }
    override def reportScalaNotes(notes: List[Note]): Unit = {
      project ! AsyncEvent(NewScalaNotesEvent(isFull = false, notes))
    }
  }

  private val reporter = new PresentationReporter(reportHandler)

  protected var scalaCompiler: RichCompilerControl = makeScalaCompiler()
  protected var initTime: Long = 0
  private var awaitingInitialCompile = true
  private var allFilesLoaded = false

  protected def bgMessage(msg: String): Unit = {
    project ! AsyncEvent(SendBackgroundMessageEvent(ProtocolConst.MsgMisc, Some(msg)))
  }

  override def preStart(): Unit = {
    bgMessage("Initializing Analyzer. Please wait...")
    initTime = System.currentTimeMillis()

    implicit val ec = context.dispatcher

    Future {
      reporter.disable()
      scalaCompiler.askNotifyWhenReady()
      if (config.sourceMode) scalaCompiler.askReloadAllFiles()
    }
  }

  override def receive = LoggingReceive {
    case x: Any =>
      try {
        process(x)
      } catch {
        case e: Exception =>
          log.error("Error during Analyzer message processing")
      }
  }

  protected def makeScalaCompiler() = new RichPresentationCompiler(
    config, settings, reporter, self, indexer, search
  )

  protected def restartCompiler(keepLoaded: Boolean): Unit = {
    val files = scalaCompiler.loadedFiles
    presCompLog.warn("Shut down old PC")
    scalaCompiler.askShutdown()
    presCompLog.warn("Starting new PC")
    scalaCompiler = makeScalaCompiler()
    if (keepLoaded) {
      presCompLog.warn("Reloading files")
      scalaCompiler.askReloadFiles(files)
    }
    scalaCompiler.askNotifyWhenReady()
    project ! AsyncEvent(CompilerRestartedEvent)
    presCompLog.warn("Started")
  }

  override def postStop(): Unit = {
    log.info("Shutting down presentation compiler")
    scalaCompiler.askClearTypeCache()
    scalaCompiler.askShutdown()
  }

  def charset: Charset = scalaCompiler.charset

  def process(msg: Any): Unit = {
    msg match {
      case ReloadExistingFilesEvent => if (allFilesLoaded) {
        presCompLog.warn("Skipping reload, in all-files mode")
      } else {
        restartCompiler(keepLoaded = true)
      }

      case FullTypeCheckCompleteEvent =>
        if (awaitingInitialCompile) {
          awaitingInitialCompile = false
          val elapsed = System.currentTimeMillis() - initTime
          log.debug("Analyzer ready in " + elapsed / 1000.0 + " seconds.")
          reporter.enable()
          project ! AsyncEvent(AnalyzerReadyEvent)
        }
        project ! AsyncEvent(FullTypeCheckCompleteEvent)

      case rpcReq: RpcRequest =>
        try {
          if (awaitingInitialCompile) {
            sender ! RPCError(ErrAnalyzerNotReady, "Analyzer is not ready! Please wait.")
          } else {
            reporter.enable()

            rpcReq match {
              case RemoveFileReq(file: File) =>
                scalaCompiler.askRemoveDeleted(file)
                sender ! VoidResponse
              case TypecheckAllReq =>
                allFilesLoaded = true
                scalaCompiler.askRemoveAllDeleted()
                scalaCompiler.askReloadAllFiles()
                scalaCompiler.askNotifyWhenReady()
                sender ! VoidResponse
              case UnloadAllReq =>
                if (config.sourceMode) {
                  log.info("in source mode, will reload all files")
                  scalaCompiler.askRemoveAllDeleted()
                  restartCompiler(keepLoaded = true)
                } else {
                  allFilesLoaded = false
                  restartCompiler(keepLoaded = false)
                }
                sender ! VoidResponse
              case TypecheckFileReq(fileInfo) =>
                handleReloadFiles(List(fileInfo), async = true)
                sender ! VoidResponse
              case TypecheckFilesReq(files) =>
                handleReloadFiles(files.map(SourceFileInfo(_)), async = false)
                sender ! VoidResponse
              case PatchSourceReq(file, edits) =>
                if (!file.exists()) {
                  sender ! RPCError(ErrFileDoesNotExist, file.getPath)
                } else {
                  val f = createSourceFile(file)
                  val revised = PatchSource.applyOperations(f, edits)
                  reporter.disable()
                  scalaCompiler.askReloadFile(revised)
                  sender ! VoidResponse
                }

              case req: PrepareRefactorReq =>
                handleRefactorPrepareRequest(req)
              case req: ExecRefactorReq =>
                handleRefactorExec(req)
              case req: CancelRefactorReq =>
                handleRefactorCancel(req)
              case CompletionsReq(fileInfo, point, maxResults, caseSens, reload) =>
                val sourcefile = createSourceFile(fileInfo)
                reporter.disable()
                val p = new OffsetPosition(sourcefile, point)
                val info = scalaCompiler.askCompletionsAt(p, maxResults, caseSens)
                sender ! info
              case UsesOfSymbolAtPointReq(file, point) =>
                val p = pos(file, point)
                sender ! scalaCompiler.askUsesOfSymAtPoint(p).map(ERangePositionHelper.fromRangePosition)
              case PackageMemberCompletionReq(path: String, prefix: String) =>
                val members = scalaCompiler.askCompletePackageMember(path, prefix)
                sender ! members
              case InspectTypeAtPointReq(file: File, range: OffsetRange) =>
                val p = pos(file, range)
                sender ! scalaCompiler.askInspectTypeAt(p)
              case InspectTypeByIdReq(id: Int) =>
                sender ! scalaCompiler.askInspectTypeById(id)
              case InspectTypeByNameReq(name: String) =>
                sender ! scalaCompiler.askInspectTypeByName(name)
              case SymbolAtPointReq(file: File, point: Int) =>
                val p = pos(file, point)
                sender ! scalaCompiler.askSymbolInfoAt(p)
              case SymbolByNameReq(typeFullName: String, memberName: Option[String], signatureString: Option[String]) =>
                sender ! scalaCompiler.askSymbolByName(typeFullName, memberName, signatureString)
              case DocUriAtPointReq(file: File, range: OffsetRange) =>
                val p = pos(file, range)
                sender ! scalaCompiler.askDocSignatureAtPoint(p)
              case DocUriForSymbolReq(typeFullName: String, memberName: Option[String], signatureString: Option[String]) =>
                sender ! scalaCompiler.askDocSignatureForSymbol(typeFullName, memberName, signatureString)
              case InspectPackageByPathReq(path: String) =>
                sender ! scalaCompiler.askPackageByPath(path)
              case TypeAtPointReq(file: File, range: OffsetRange) =>
                val p = pos(file, range)
                sender ! scalaCompiler.askTypeInfoAt(p)
              case TypeByIdReq(id: Int) =>
                sender ! scalaCompiler.askTypeInfoById(id)
              case TypeByNameReq(name: String) =>
                sender ! scalaCompiler.askTypeInfoByName(name)
              case TypeByNameAtPointReq(name: String, file: File, range: OffsetRange) =>
                val p = pos(file, range)
                sender ! scalaCompiler.askTypeInfoByNameAt(name, p)
              case CallCompletionReq(id: Int) =>
                sender ! scalaCompiler.askCallCompletionInfoById(id)
              case SymbolDesignationsReq(f, start, end, tpes) =>
                if (!FileUtils.isScalaSourceFile(f)) {
                  sender ! SymbolDesignations(f, List.empty)
                } else {
                  val sf = createSourceFile(f)
                  val clampedEnd = math.max(end, start)
                  val pos = new RangePosition(sf, start, start, clampedEnd)
                  if (tpes.nonEmpty) {
                    val syms = scalaCompiler.askSymbolDesignationsInRegion(pos, tpes)
                    sender ! syms
                  } else {
                    sender ! SymbolDesignations(file(sf.path), List.empty)
                  }
                }
              case ExpandSelectionReq(file, start: Int, stop: Int) =>
                sender ! handleExpandselection(file, start, stop)
              case FormatSourceReq(files: List[File]) =>
                handleFormatFiles(files)
                sender ! VoidResponse
              case FormatOneSourceReq(fileInfo: SourceFileInfo) =>
                sender ! handleFormatFile(fileInfo)

              case unexpected =>
                // TODO compiler blew up... too many missing cases
                require(requirement = false, unexpected.toString)
            }
          }
        } catch {
          case e: Throwable =>
            log.error(e, "Error handling RPC: " + e)
            sender ! RPCError(ErrExceptionInAnalyzer, "Error occurred in Analyzer. Check the server log.")
        }

      case DoExecUndo(undo) =>
        try {
          sender ! handleExecUndo(undo)
        } catch {
          case e: Throwable =>
            log.error(e, "Error handling RPC: " + e)
            sender ! RPCError(ErrExceptionInAnalyzer, "Error occurred in Analyzer. Check the server log.")
        }

      case other =>
        log.error("Unknown message type: " + other)
    }
  }

  def handleReloadFiles(files: List[SourceFileInfo], async: Boolean): Unit = {
    files foreach { file =>
      require(file.file.exists, "" + file + " does not exist")
    }

    val (javas, scalas) = files.filter(_.file.exists).partition(
      _.file.getName.endsWith(".java")
    )

    if (scalas.nonEmpty) {
      val sourceFiles = scalas.map(createSourceFile)
      scalaCompiler.askReloadFiles(sourceFiles)
      scalaCompiler.askNotifyWhenReady()
      if (!async)
        sourceFiles.foreach(scalaCompiler.askLoadedTyped)
    }
  }

  def pos(file: File, range: OffsetRange): OffsetPosition = {
    val f = scalaCompiler.createSourceFile(file.canon.getPath)
    if (range.from == range.to) new OffsetPosition(f, range.from)
    else new RangePosition(f, range.from, range.from, range.to)
  }

  def pos(file: File, offset: Int): OffsetPosition = {
    val f = scalaCompiler.createSourceFile(file.canon.getPath)
    new OffsetPosition(f, offset)
  }

  def createSourceFile(file: File): SourceFile = {
    scalaCompiler.createSourceFile(file.canon.getPath)
  }

  def createSourceFile(file: SourceFileInfo): SourceFile = {
    scalaCompiler.createSourceFile(file)
  }

}

