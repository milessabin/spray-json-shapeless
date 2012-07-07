/**
*  Copyright (c) 2010, Aemon Cannon
*  All rights reserved.
*  
*  Redistribution and use in source and binary forms, with or without
*  modification, are permitted provided that the following conditions are met:
*      * Redistributions of source code must retain the above copyright
*        notice, this list of conditions and the following disclaimer.
*      * Redistributions in binary form must reproduce the above copyright
*        notice, this list of conditions and the following disclaimer in the
*        documentation and/or other materials provided with the distribution.
*      * Neither the name of ENSIME nor the
*        names of its contributors may be used to endorse or promote products
*        derived from this software without specific prior written permission.
*  
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
*  DISCLAIMED. IN NO EVENT SHALL Aemon Cannon BE LIABLE FOR ANY
*  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package org.ensime.server
import java.io.File
import org.ensime.config.ProjectConfig
import org.ensime.protocol.ProtocolConversions
import org.ensime.protocol.ProtocolConst._
import org.ensime.util._
import scala.actors._
import scala.actors.Actor._
import scala.collection.{ Iterable, Map }
import scala.tools.nsc.{ Settings }

import scala.tools.nsc.interactive.{
  Global,
  BuildManager,
  SimpleBuildManager,
  RefinedBuildManager
}
import scala.tools.nsc.io.{ AbstractFile }
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
        super.computeInternalPhases
        phasesSet += dependencyAnalysis
      }
      def newRun() = new Run()
    }
    override protected def newCompiler(settings: Settings) = new BuilderGlobal(settings, reporter)
  }

  import protocol._

  private val settings = new Settings(Console.println)
  settings.processArguments(config.builderArgs, false)
  private val reporter = new PresentationReporter(new ReportHandler{
      override def messageUser(str:String){
	project ! AsyncEvent(toWF(
	  SendBackgroundMessageEvent(MsgCompilerUnexpectedError, Some(str))))
      }
    })
  private val bm: BuildManager = new IncrementalBuildManager(settings, reporter)

  import bm._

  def act() {

    loop {
      try {
        receive {
          case BuilderShutdownEvent => {
            exit('stop)
          }
          case RPCRequestEvent(req: Any, callId: Int) => {
            try {
              req match {

                case RebuildAllReq() => {
                  project ! AsyncEvent(toWF(SendBackgroundMessageEvent(
		    MsgBuildingEntireProject, Some("Building entire project. Please wait..."))))
                  val files = config.sourceFilenames.map(s => AbstractFile.getFile(s))
                  reporter.reset
                  bm.addSourceFiles(files)
                  project ! AsyncEvent(toWF(SendBackgroundMessageEvent(
		    MsgBuildComplete, Some("Build complete."))))
                  val result = toWF(reporter.allNotes.map(toWF))
                  project ! RPCResultEvent(result, callId)
                }
                case AddSourceFilesReq(files: Iterable[File]) => {
                  val fileSet = files.map(AbstractFile.getFile(_)).toSet
                  bm.addSourceFiles(fileSet)
                  val result = toWF(reporter.allNotes.map(toWF))
                  project ! RPCResultEvent(result, callId)
                }
                case RemoveSourceFilesReq(files: Iterable[File]) => {
                  val fileSet = files.map(AbstractFile.getFile(_)).toSet
                  project ! RPCResultEvent(toWF(true), callId)
                  reporter.reset
                  bm.removeFiles(fileSet)
                  val result = toWF(reporter.allNotes.map(toWF))
                  project ! RPCResultEvent(result, callId)
                }
                case UpdateSourceFilesReq(files: Iterable[File]) => {
                  val fileSet = files.map(AbstractFile.getFile(_)).toSet
                  reporter.reset
                  bm.update(fileSet, Set())
                  val result = toWF(reporter.allNotes.map(toWF))
                  project ! RPCResultEvent(result, callId)
                }

              }
            } catch {
              case e: Exception =>
              {
                System.err.println("Error handling RPC: " +
                  e + " :\n" +
                  e.getStackTraceString)

                project.sendRPCError(ErrExceptionInBuilder,
		  Some("Error occurred in incremental builder. Check the server log."), 
		  callId)
              }
            }
          }
          case other =>
          {
            println("Incremental Builder: WTF, what's " + other)
          }
        }

      } catch {
        case e: Exception =>
        {
          System.err.println("Error at Incremental Builder message loop: " + e + " :\n" + e.getStackTraceString)
        }
      }
    }
  }

  override def finalize() {
    System.out.println("Finalizing incremental builder actor.")
  }

}

