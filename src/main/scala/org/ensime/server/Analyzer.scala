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
  val protocol: ProtocolConversions,
  val config: EnsimeConfig)
    extends Actor with ActorLogging with RefactoringHandler {

  private val presCompLog = LoggerFactory.getLogger(classOf[Global])
  private val settings = new Settings(presCompLog.error)
  settings.YpresentationDebug.value = presCompLog.isTraceEnabled
  settings.YpresentationVerbose.value = presCompLog.isDebugEnabled
  settings.verbose.value = presCompLog.isDebugEnabled
  settings.usejavacp.value = false

  // HACK: should really get scala-library from our classpath
  //       and give the option to disable the scala-library (e.g. when working
  //       on the standard library!)
  val scalaLib = config.allJars.find(_.getName.contains("scala-library")).get
  settings.bootclasspath.value = scalaLib.getAbsolutePath
  settings.classpath.value = config.compileClasspath.mkString(File.pathSeparator)

  settings.processArguments(config.compilerArgs, processAll = false)

  log.info("Presentation Compiler settings:\n" + settings)

  import protocol._

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

  protected val scalaCompiler: RichCompilerControl = new RichPresentationCompiler(
    config, settings, reporter, self, indexer, search
  )

  protected var initTime: Long = 0
  private var awaitingInitialCompile = true
  private var allFilesLoaded = false

  import scalaCompiler._

  override def preStart(): Unit = {
    project.bgMessage("Initializing Analyzer. Please wait...")
    initTime = System.currentTimeMillis()

    implicit val ec = context.dispatcher

    Future {
      reporter.disable()
      scalaCompiler.askNotifyWhenReady()
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

  def process(msg: Any): Unit = {
    msg match {
      case AnalyzerShutdownEvent =>
        scalaCompiler.askClearTypeCache()
        scalaCompiler.askShutdown()
        context.stop(self)

      case ReloadExistingFilesEvent => if (!allFilesLoaded) {
        scalaCompiler.askInvalidateTargets()
        scalaCompiler.askRemoveAllDeleted()
        scalaCompiler.askReloadExistingFiles()
      } else {
        presCompLog.warn("Skipping, in all-files mode")
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

      case rpcReq @ RPCRequestEvent(req: Any, callId: Int) =>
        try {
          if (awaitingInitialCompile) {
            project.sendRPCError(ErrAnalyzerNotReady, "Analyzer is not ready! Please wait.", callId)
          } else {
            reporter.enable()

            req match {
              case RemoveFileReq(file: File) =>
                askRemoveDeleted(file)
                project ! RPCResultEvent(toWF(value = true), callId)

              case ReloadAllReq =>
                allFilesLoaded = true
                scalaCompiler.askRemoveAllDeleted()
                scalaCompiler.askReloadAllFiles()
                scalaCompiler.askNotifyWhenReady()
                project ! RPCResultEvent(toWF(value = true), callId)

              case UnloadAllReq =>
                allFilesLoaded = false
                scalaCompiler.askInvalidateTargets()
                scalaCompiler.askRemoveAllDeleted()
                scalaCompiler.askUnloadAllFiles()
                project ! RPCResultEvent(toWF(value = true), callId)

              case ReloadFilesReq(files) =>
                val (javas, scalas) = files.filter(_.file.exists).partition(
                  _.file.getName.endsWith(".java"))

                if (scalas.nonEmpty) {
                  scalaCompiler.askReloadFiles(scalas.map(createSourceFile))
                  scalaCompiler.askNotifyWhenReady()
                  project ! RPCResultEvent(toWF(value = true), callId)
                }

              case PatchSourceReq(file, edits) =>
                if (!file.exists()) {
                  project.sendRPCError(ErrFileDoesNotExist, file.getPath, callId)
                } else {
                  val f = createSourceFile(file)
                  val revised = PatchSource.applyOperations(f, edits)
                  reporter.disable()
                  scalaCompiler.askReloadFile(revised)
                  project ! RPCResultEvent(toWF(value = true), callId)
                }

              case req: RefactorPrepareReq =>
                handleRefactorPrepareRequest(req, callId)

              case req: RefactorExecReq =>
                handleRefactorExec(req, callId)

              case req: RefactorCancelReq =>
                handleRefactorCancel(req, callId)

              case CompletionsReq(file: File, point: Int,
                maxResults: Int, caseSens: Boolean, reload: Boolean) =>
                val p = if (reload) pos(file, point) else posNoRead(file, point)
                reporter.disable()
                val info = scalaCompiler.askCompletionsAt(
                  p, maxResults, caseSens)
                project ! RPCResultEvent(toWF(info), callId)

              case ImportSuggestionsReq(_, _, _, _) =>
                indexer ! rpcReq

              case PublicSymbolSearchReq(_, _) =>
                indexer ! rpcReq

              case UsesOfSymAtPointReq(file: File, point: Int) =>
                val p = pos(file, point)
                val uses = scalaCompiler.askUsesOfSymAtPoint(p)
                project ! RPCResultEvent(toWF(uses.map(toWF)), callId)

              case PackageMemberCompletionReq(path: String, prefix: String) =>
                val members = scalaCompiler.askCompletePackageMember(path, prefix)
                project ! RPCResultEvent(toWF(members.map(toWF)), callId)

              case InspectTypeReq(file: File, range: OffsetRange) =>
                val p = pos(file, range)
                val result = scalaCompiler.askInspectTypeAt(p) match {
                  case Some(info) => toWF(info)
                  case None => wfNull
                }
                project ! RPCResultEvent(result, callId)

              case InspectTypeByIdReq(id: Int) =>
                val result = scalaCompiler.askInspectTypeById(id) match {
                  case Some(info) => toWF(info)
                  case None => wfNull
                }
                project ! RPCResultEvent(result, callId)

              case SymbolAtPointReq(file: File, point: Int) =>
                val p = pos(file, point)
                val result = scalaCompiler.askSymbolInfoAt(p) match {
                  case Some(info) => toWF(info)
                  case None => wfNull
                }
                project ! RPCResultEvent(result, callId)

              case InspectPackageByPathReq(path: String) =>
                val result = scalaCompiler.askPackageByPath(path) match {
                  case Some(info) => toWF(info)
                  case None => wfNull
                }
                project ! RPCResultEvent(result, callId)

              case TypeAtPointReq(file: File, range: OffsetRange) =>
                val p = pos(file, range)
                val result = scalaCompiler.askTypeInfoAt(p) match {
                  case Some(info) => toWF(info)
                  case None => wfNull
                }
                project ! RPCResultEvent(result, callId)

              case TypeByIdReq(id: Int) =>
                val result = scalaCompiler.askTypeInfoById(id) match {
                  case Some(info) => toWF(info)
                  case None => wfNull
                }
                project ! RPCResultEvent(result, callId)

              case TypeByNameReq(name: String) =>
                val result = scalaCompiler.askTypeInfoByName(name) match {
                  case Some(info) => toWF(info)
                  case None => wfNull
                }
                project ! RPCResultEvent(result, callId)

              case TypeByNameAtPointReq(name: String, file: File, range: OffsetRange) =>
                val p = pos(file, range)
                val result = scalaCompiler.askTypeInfoByNameAt(name, p) match {
                  case Some(info) => toWF(info)
                  case None => wfNull
                }

                project ! RPCResultEvent(result, callId)

              case CallCompletionReq(id: Int) =>
                val result = scalaCompiler.askCallCompletionInfoById(id) match {
                  case Some(info) => toWF(info)
                  case None => wfNull
                }
                project ! RPCResultEvent(result, callId)

              case SymbolDesignationsReq(file, start, end, tpes) =>
                if (!FileUtils.isScalaSourceFile(file)) {
                  project ! RPCResultEvent(
                    toWF(SymbolDesignations(file.getPath, List.empty)), callId)
                } else {
                  val f = createSourceFile(file)
                  val clampedEnd = math.max(end, start)
                  val pos = new RangePosition(f, start, start, clampedEnd)
                  if (tpes.nonEmpty) {
                    val syms = scalaCompiler.askSymbolDesignationsInRegion(
                      pos, tpes)
                    project ! RPCResultEvent(toWF(syms), callId)
                  } else {
                    project ! RPCResultEvent(
                      toWF(SymbolDesignations(f.path, List.empty)), callId)
                  }
                }
            }
          }
        } catch {
          case e: Throwable =>
            log.error(e, "Error handling RPC: " + e)
            project.sendRPCError(ErrExceptionInAnalyzer, "Error occurred in Analyzer. Check the server log.", callId)
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

