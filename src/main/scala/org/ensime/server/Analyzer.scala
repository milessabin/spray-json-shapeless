package org.ensime.server
import java.io.File
import org.ensime.config.ProjectConfig
import org.ensime.model._
import org.ensime.protocol.ProtocolConversions
import org.ensime.util._
import org.ensime.util.RichFile._
import scala.actors._
import scala.actors.Actor._
import scala.collection.{ Iterable }
import scala.tools.nsc.{ Settings }
import scala.tools.nsc.ast._
import scala.tools.nsc.util.{ OffsetPosition }

case class FullTypeCheckCompleteEvent()

class Analyzer(val project: Project, val protocol: ProtocolConversions, val config: ProjectConfig)
  extends Actor with RefactoringController with JavaCompiling {

  private val settings = new Settings(Console.println)
  settings.processArguments(config.compilerArgs, false)
  settings.usejavacp.value = false
  private val reporter = new PresentationReporter()
  protected val scalaCompiler: RichCompilerControl = new RichPresentationCompiler(
    settings, reporter, this, config)
  protected var awaitingInitialCompile = true

  import scalaCompiler._
  import protocol._

  def act() {

    project ! SendBackgroundMessageEvent("Initializing Analyzer. Please wait...")

    println("Building Java sources...")
    rebuildJavaUnits
    val units = javaUnitForFile.values
    if (!(units.isEmpty)) {
      javaCompiler.compile(units.toArray)
    }

    println("Building Scala sources...")
    scalaCompiler.askNewRunnerThread
    scalaCompiler.askReloadAllFiles

    loop {
      try {
        receive {
          case AnalyzerShutdownEvent() => {
            javaCompiler.reset
            javaUnitForFile.clear

            scalaCompiler.askClearTypeCache
            scalaCompiler.askShutdown()
            exit('stop)
          }

          case FullTypeCheckCompleteEvent() => {
            if (awaitingInitialCompile) {
              project ! AnalyzerReadyEvent()
              awaitingInitialCompile = false
            }
            val result = NoteList('scala, true, reporter.allNotes)
            project ! TypeCheckResultEvent(result)
          }

          case RPCRequestEvent(req: Any, callId: Int) => {
            try {
              if (awaitingInitialCompile) {
                project ! RPCErrorEvent(
                  "Analyzer is not ready! Please wait.", callId)
              } else {
                req match {
                  case ReloadAllReq() => {
                    javaCompiler.reset
                    javaCompiler.compile(javaUnitForFile.values.toArray)
                    val result = NoteList('java, true, javaNotes)
                    project ! TypeCheckResultEvent(result)
                    scalaCompiler.askReloadAllFiles()
                    project ! RPCResultEvent(toWF(true), callId)
                  }

                  case ReloadFileReq(file: File) => {
                    for (u <- javaUnitForFile.get(file.getCanonicalPath)) {
                      javaCompiler.compile(Array(u))
                      val result = NoteList('java, false, javaNotes)
                      project ! TypeCheckResultEvent(result)
                    }
                    val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
                    scalaCompiler.askReloadFile(f)
                    val result = NoteList('scala, false, reporter.allNotes)
                    project ! TypeCheckResultEvent(result)
                    project ! RPCResultEvent(toWF(true), callId)
                  }

                  case req: RefactorPerformReq => {
                    handleRefactorRequest(req, callId)
                  }

                  case req: RefactorExecReq => {
                    handleRefactorExec(req, callId)
                  }

                  case req: RefactorCancelReq => {
                    handleRefactorCancel(req, callId)
                  }

                  case RemoveFileReq(file: File) => {
                    val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
                    scalaCompiler.removeUnitOf(f)
                  }

                  case ScopeCompletionReq(file: File, point: Int,
                    prefix: String, constructor: Boolean) => {
                    val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
                    val p = new OffsetPosition(f, point)
                    val syms = scalaCompiler.askCompleteSymbolAt(p, prefix, constructor)
                    project ! RPCResultEvent(toWF(syms.map(toWF)), callId)
                  }

                  case TypeCompletionReq(file: File, point: Int, prefix: String) => {
                    val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
                    val p = new OffsetPosition(f, point)
                    val members = scalaCompiler.askCompleteMemberAt(p, prefix)
                    project ! RPCResultEvent(toWF(members.map(toWF)), callId)
                  }

                  case InspectTypeReq(file: File, point: Int) => {
                    val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
                    val p = new OffsetPosition(f, point)
                    val inspectInfo = scalaCompiler.askInspectTypeAt(p)
                    project ! RPCResultEvent(toWF(inspectInfo), callId)
                  }

                  case InspectTypeByIdReq(id: Int) => {
                    val inspectInfo = scalaCompiler.askInspectTypeById(id)
                    project ! RPCResultEvent(toWF(inspectInfo), callId)
                  }

                  case SymbolAtPointReq(file: File, point: Int) => {
                    val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
                    val p = new OffsetPosition(f, point)
                    val info = scalaCompiler.askSymbolInfoAt(p)
                    project ! RPCResultEvent(toWF(info), callId)
                  }

                  case InspectPackageByPathReq(path: String) => {
                    val packageInfo = scalaCompiler.askPackageByPath(path)
                    project ! RPCResultEvent(toWF(packageInfo), callId)
                  }

                  case TypeAtPointReq(file: File, point: Int) => {
                    val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
                    val p = new OffsetPosition(f, point)
                    val typeInfo = scalaCompiler.askTypeInfoAt(p)
                    project ! RPCResultEvent(toWF(typeInfo), callId)
                  }

                  case TypeByIdReq(id: Int) => {
                    val tpeInfo = scalaCompiler.askTypeInfoById(id)
                    project ! RPCResultEvent(toWF(tpeInfo), callId)
                  }

                  case CallCompletionReq(id: Int) => {
                    val callInfo = scalaCompiler.askCallCompletionInfoById(id)
                    project ! RPCResultEvent(toWF(callInfo), callId)
                  }
                }
              }
            } catch {
              case e: Exception => {
                System.err.println("Error handling RPC: " + e + " :\n" +
                  e.getStackTraceString)
                project ! RPCErrorEvent("Error occurred in Analyzer. Check the server log.", callId)
              }
            }
          }
          case other => {
            println("Analyzer: WTF, what's " + other)
          }
        }

      } catch {
        case e: Exception => {
          System.err.println("Error at Compiler message loop: " + e + " :\n" + e.getStackTraceString)
        }
      }
    }
  }

  override def finalize() {
    System.out.println("Finalizing compilation actor.")
  }

}

