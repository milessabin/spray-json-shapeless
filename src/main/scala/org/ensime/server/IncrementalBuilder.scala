package org.ensime.server

import java.io.File
import org.ensime.config.ProjectConfig
import org.ensime.protocol.ProtocolConversions
import org.ensime.protocol.ProtocolConst._
import org.ensime.util._
import scala.actors._
import scala.collection.{ mutable, Iterable }
import scala.tools.nsc.Settings

import scala.tools.nsc.interactive.{ BuildManager, RefinedBuildManager }
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.Reporter

case class BuilderShutdownEvent()

case class RebuildAllReq()
case class AddSourceFilesReq(files: Iterable[File])
case class RemoveSourceFilesReq(files: Iterable[File])
case class UpdateSourceFilesReq(files: Iterable[File])

class IncrementalBuilder(project: Project, protocol: ProtocolConversions, config: ProjectConfig) extends Actor {

  class IncrementalBuildManager(settings: Settings, reporter: Reporter)
      extends RefinedBuildManager(settings) {
    class IncrementalGlobal(settings: Settings, reporter: Reporter)
        extends scala.tools.nsc.Global(settings, reporter) {
      override def computeInternalPhases() {
        super.computeInternalPhases()
        phasesSet += dependencyAnalysis
      }
      def newRun() = new Run()
    }
    override protected def newCompiler(settings: Settings) = new BuilderGlobal(settings, reporter)
  }

  import protocol._

  private val settings = new Settings(Console.println)
  settings.processArguments(config.builderArgs, processAll = false)

  private val reportHandler = new ReportHandler {
    override def messageUser(str: String) {
      project ! AsyncEvent(toWF(
        SendBackgroundMessageEvent(MsgCompilerUnexpectedError, Some(str))))
    }
    private val notes = new mutable.HashSet[Note] with mutable.SynchronizedSet[Note]
    def allNotes: Iterable[Note] = notes.toList
    override def reportScalaNotes(n: List[Note]) {
      notes ++= n
    }
    override def clearAllScalaNotes() {
      notes.clear()
    }
  }

  private val reporter = new PresentationReporter(reportHandler)

  private val bm: BuildManager = new IncrementalBuildManager(settings, reporter)

  def act() {

    loop {
      try {
        receive {
          case BuilderShutdownEvent =>
            exit('stop)
          case RPCRequestEvent(req: Any, callId: Int) =>
            try {
              req match {

                case RebuildAllReq() =>
                  project ! AsyncEvent(toWF(SendBackgroundMessageEvent(
                    MsgBuildingEntireProject, Some("Building entire project. Please wait..."))))
                  val files = config.sourceFilenames.map(s => AbstractFile.getFile(s))
                  reporter.reset()
                  bm.addSourceFiles(files)
                  project ! AsyncEvent(toWF(SendBackgroundMessageEvent(
                    MsgBuildComplete, Some("Build complete."))))
                  val result = toWF(reportHandler.allNotes.map(toWF))
                  project ! RPCResultEvent(result, callId)
                case AddSourceFilesReq(files) =>
                  val fileSet = files.map(AbstractFile.getFile(_)).toSet
                  bm.addSourceFiles(fileSet)
                  val result = toWF(reportHandler.allNotes.map(toWF))
                  project ! RPCResultEvent(result, callId)
                case RemoveSourceFilesReq(files) =>
                  val fileSet = files.map(AbstractFile.getFile(_)).toSet
                  project ! RPCResultEvent(toWF(value = true), callId)
                  reporter.reset()
                  bm.removeFiles(fileSet)
                  val result = toWF(reportHandler.allNotes.map(toWF))
                  project ! RPCResultEvent(result, callId)
                case UpdateSourceFilesReq(files) =>
                  val fileSet = files.map(AbstractFile.getFile(_)).toSet
                  reporter.reset()
                  bm.update(fileSet, Set())
                  val result = toWF(reportHandler.allNotes.map(toWF))
                  project ! RPCResultEvent(result, callId)

              }
            } catch {
              case e: Exception =>
                System.err.println("Error handling RPC: " +
                  e + " :\n" +
                  e.getStackTraceString)

                project.sendRPCError(ErrExceptionInBuilder,
                  Some("Error occurred in incremental builder. Check the server log."),
                  callId)
            }
          case other =>
            println("Incremental Builder: WTF, what's " + other)
        }

      } catch {
        case e: Exception =>
          System.err.println("Error at Incremental Builder message loop: " + e + " :\n" + e.getStackTraceString)
      }
    }
  }

  override def finalize() {
    System.out.println("Finalizing incremental builder actor.")
  }

}

