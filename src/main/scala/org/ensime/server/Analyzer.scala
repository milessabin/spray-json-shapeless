package org.ensime.server

import java.io.File
import akka.actor.{ ActorLogging, Actor, ActorRef }
import org.ensime.config._
import org.ensime.indexer.SearchService
import org.ensime.model._
import org.ensime.protocol._
import org.ensime.protocol.ProtocolConst._
import org.ensime.util._
import org.slf4j.LoggerFactory
import scala.concurrent.Future
import scala.reflect.internal.util.RangePosition
import scala.tools.nsc.Settings
import scala.reflect.internal.util.OffsetPosition
import scala.tools.nsc.interactive.Global

case class CompilerFatalError(e: Throwable)

class Analyzer(
  val project: Project,
  val indexer: ActorRef,
  search: SearchService,
  val config: EnsimeConfig)
    extends Actor with ActorLogging with RefactoringHandler {

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
    override def messageUser(str: String) {
      project ! AsyncEvent(SendBackgroundMessageEvent(MsgCompilerUnexpectedError, Some(str)))
    }
    override def clearAllScalaNotes() {
      project ! AsyncEvent(ClearAllScalaNotesEvent)
    }
    override def clearAllJavaNotes() {
      project ! AsyncEvent(ClearAllJavaNotesEvent)
    }
    override def reportScalaNotes(notes: List[Note]) {
      project ! AsyncEvent(NewScalaNotesEvent(NoteList(full = false, notes)))
    }
    override def reportJavaNotes(notes: List[Note]) {
      project ! AsyncEvent(NewJavaNotesEvent(NoteList(full = false, notes)))
    }
  }

  private val reporter = new PresentationReporter(reportHandler)

  protected var scalaCompiler: RichCompilerControl = makeScalaCompiler()
  protected var initTime: Long = 0
  private var awaitingInitialCompile = true
  private var allFilesLoaded = false

  override def preStart(): Unit = {
    project.bgMessage("Initializing Analyzer. Please wait...")
    initTime = System.currentTimeMillis()

    implicit val ec = context.dispatcher

    Future {
      reporter.disable()
      scalaCompiler.askNotifyWhenReady()
      if (config.sourceMode) scalaCompiler.askReloadAllFiles()
    }
  }

  override def receive = {
    case x: Any =>
      try {
        process(x)
      } catch {
        case e: Exception =>
          log.error("Error during Analyzer message processing")
      }
  }

  protected def makeScalaCompiler() = new RichPresentationCompiler(
    config, settings, reporter, self, indexer, search)

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

  def process(msg: Any): Unit = {
    msg match {
      case AnalyzerShutdownEvent =>
        scalaCompiler.askClearTypeCache()
        scalaCompiler.askShutdown()
        context.stop(self)

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

      case rpcReq: RPCRequest =>
        try {
          if (awaitingInitialCompile) {
            sender ! RPCError(ErrAnalyzerNotReady, "Analyzer is not ready! Please wait.")
          } else {
            reporter.enable()

            rpcReq match {
              case RemoveFileReq(file: File) =>
                scalaCompiler.askRemoveDeleted(file)
                sender ! VoidResponse
              case ReloadAllReq =>
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
              case ReloadFilesReq(files) =>
                files foreach { file =>
                  require(file.file.exists, file + " does not exist")
                }

                val (javas, scalas) = files.filter(_.file.exists).partition(
                  _.file.getName.endsWith(".java"))

                if (scalas.nonEmpty) {
                  scalaCompiler.askReloadFiles(scalas.map(createSourceFile))
                  scalaCompiler.askNotifyWhenReady()
                }
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

              case req: RefactorPrepareReq =>
                handleRefactorPrepareRequest(req)
              case req: RefactorExecReq =>
                handleRefactorExec(req)
              case req: RefactorCancelReq =>
                handleRefactorCancel(req)
              case CompletionsReq(file: File, point: Int,
                maxResults: Int, caseSens: Boolean, reload: Boolean) =>
                val p = if (reload) pos(file, point) else posNoRead(file, point)
                reporter.disable()
                val info = scalaCompiler.askCompletionsAt(p, maxResults, caseSens)
                sender ! info
              case UsesOfSymAtPointReq(file: File, point: Int) =>
                val p = pos(file, point)
                sender ! scalaCompiler.askUsesOfSymAtPoint(p).map(ERangePosition.apply)
              case PackageMemberCompletionReq(path: String, prefix: String) =>
                val members = scalaCompiler.askCompletePackageMember(path, prefix)
                sender ! members
              case InspectTypeReq(file: File, range: OffsetRange) =>
                val p = pos(file, range)
                sender ! scalaCompiler.askInspectTypeAt(p)
              case InspectTypeByIdReq(id: Int) =>
                sender ! scalaCompiler.askInspectTypeById(id)
              case InspectTypeByNameReq(name: String) =>
                sender ! scalaCompiler.askInspectTypeByName(name)
              case SymbolAtPointReq(file: File, point: Int) =>
                val p = pos(file, point)
                sender ! scalaCompiler.askSymbolInfoAt(p)
              case InspectPackageByPathReq(path: String) =>
                sender ! scalaCompiler.askPackageByPath(path)
              case TypeAtPointReq(file: File, range: OffsetRange) =>
                val p = pos(file, range)
                sender ! scalaCompiler.askTypeInfoAt(p)
              case TypeByIdReq(id: Int) =>
                sender ! scalaCompiler.askTypeInfoById(id)
              case MemberByNameReq(typeName: String, memberName: String, memberIsType: Boolean) =>
                sender ! scalaCompiler.askMemberInfoByName(typeName, memberName, memberIsType)
              case TypeByNameReq(name: String) =>
                sender ! scalaCompiler.askTypeInfoByName(name)
              case TypeByNameAtPointReq(name: String, file: File, range: OffsetRange) =>
                val p = pos(file, range)
                sender ! scalaCompiler.askTypeInfoByNameAt(name, p)
              case CallCompletionReq(id: Int) =>
                sender ! scalaCompiler.askCallCompletionInfoById(id)
              case SymbolDesignationsReq(file, start, end, tpes) =>
                if (!FileUtils.isScalaSourceFile(file)) {
                  sender ! SymbolDesignations(file.getPath, List.empty)
                } else {
                  val f = createSourceFile(file)
                  val clampedEnd = math.max(end, start)
                  val pos = new RangePosition(f, start, start, clampedEnd)
                  if (tpes.nonEmpty) {
                    val syms = scalaCompiler.askSymbolDesignationsInRegion(
                      pos, tpes)
                    sender ! syms
                  } else {
                    sender ! SymbolDesignations(f.path, List.empty)
                  }
                }
            }
          }
        } catch {
          case e: Throwable =>
            log.error(e, "Error handling RPC: " + e)
            sender ! RPCError(ErrExceptionInAnalyzer, "Error occurred in Analyzer. Check the server log.")
        }

      case other =>
        log.error("Unknown message type: " + other)
    }
  }

  def pos(file: File, range: OffsetRange) = {
    val f = scalaCompiler.createSourceFile(file.getCanonicalPath)
    if (range.from == range.to) new OffsetPosition(f, range.from)
    else new RangePosition(f, range.from, range.from, range.to)
  }

  def pos(file: File, offset: Int) = {
    val f = scalaCompiler.createSourceFile(file.getCanonicalPath)
    new OffsetPosition(f, offset)
  }

  def posNoRead(file: File, offset: Int) = {
    val f = scalaCompiler.findSourceFile(file.getCanonicalPath).get
    new OffsetPosition(f, offset)
  }

  def createSourceFile(file: File) = {
    scalaCompiler.createSourceFile(file.getCanonicalPath)
  }

  def createSourceFile(file: SourceFileInfo) = {
    scalaCompiler.createSourceFile(file)
  }
}

