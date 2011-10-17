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
import org.ensime.util.RichFile._
import scala.actors._
import scala.actors.Actor._
import scala.collection.{ Iterable }
import scala.collection.mutable.{ ListBuffer }
import scala.tools.nsc.util.RangePosition
import scala.tools.nsc.{ Settings }
import scala.tools.nsc.ast._
import scala.tools.nsc.util.{ OffsetPosition }

case class FullTypeCheckCompleteEvent()
case class CompilerFatalError(e: Throwable)

class Analyzer(val project: Project, val protocol: ProtocolConversions, val config: ProjectConfig)
extends Actor with RefactoringHandler {

  private val settings = new Settings(Console.println)
  settings.processArguments(config.compilerArgs, false)
  settings.usejavacp.value = false

  println("\nPresentation Compiler settings:")
  println(settings.toString)
  println("")

  private val reportHandler: ReportHandler = new ReportHandler {
    override def messageUser(str: String) {
      project ! SendBackgroundMessageEvent(MsgCompilerUnexpectedError, Some(str))
    }
    override def clearAllScalaNotes() {
      project ! ClearAllNotesEvent('scala)
    }
    override def clearAllJavaNotes() {
      project ! ClearAllNotesEvent('java)
    }
    override def reportScalaNotes(notes: List[Note]) {
      project ! NewNotesEvent('scala, NoteList(false, notes))
    }
    override def reportJavaNotes(notes: List[Note]) {
      project ! NewNotesEvent('java, NoteList(false, notes))
    }
  }

  private val reporter = new PresentationReporter(reportHandler)

  protected val indexer: Actor = new Indexer(project, protocol, config)

  protected val scalaCompiler: RichCompilerControl = new RichPresentationCompiler(
    settings, reporter, this, indexer, config)
  protected val javaCompiler: JavaCompiler = new JavaCompiler(config, reportHandler, indexer)
  protected var awaitingInitialCompile = true

  import protocol._
  import scalaCompiler._

  def act() {
    project ! SendBackgroundMessageEvent(
      MsgInitializingAnalyzer, Some("Initializing Analyzer. Please wait..."))

    println("Initing Indexer...")
    indexer.start
    if (!config.disableIndexOnStartup) {
      indexer ! RebuildStaticIndexReq()
    }

    println("Building Java sources...")
    javaCompiler.compileAll()

    println("Building Scala sources...")
    reporter.disable()
    scalaCompiler.askReloadAllFiles()
    scalaCompiler.askNotifyWhenReady()

    loop {
      try {
        receive {
          case AnalyzerShutdownEvent() => {
            javaCompiler.shutdown()
            scalaCompiler.askClearTypeCache()
            scalaCompiler.askShutdown()
            indexer ! IndexerShutdownReq()
            exit('stop)
          }

          case FullTypeCheckCompleteEvent() => {
            if (awaitingInitialCompile) {
              awaitingInitialCompile = false
              reporter.enable()
              project ! AnalyzerReadyEvent()
            }
            project ! FullTypeCheckCompleteEvent()
          }

          case rpcReq @ RPCRequestEvent(req: Any, callId: Int) => {
            try {
              if (awaitingInitialCompile) {
                project ! RPCErrorEvent(ErrAnalyzerNotReady,
                  Some("Analyzer is not ready! Please wait."), callId)
              } else {
                req match {

                  case RemoveFileReq(file: File) => {
                    askRemoveDeleted(file)
                    project ! RPCResultEvent(toWF(true), callId)
                  }

                  case ReloadAllReq() => {
                    javaCompiler.reset()
                    javaCompiler.compileAll()
                    scalaCompiler.askRemoveAllDeleted()
                    scalaCompiler.askReloadAllFiles()
                    scalaCompiler.askNotifyWhenReady()
                    project ! RPCResultEvent(toWF(true), callId)
                  }

                  case ReloadFileReq(file: File) => {
                    if (!file.exists()) {
                      project ! RPCErrorEvent(ErrFileDoesNotExist,
                        Some(file.getPath()), callId)
                    } else {
                      if (file.getAbsolutePath().endsWith(".java")) {
                        javaCompiler.compileFile(file)
                      }
                      val f = sourceFile(file)
                      scalaCompiler.askReloadFile(f)
                      scalaCompiler.askNotifyWhenReady()
                      project ! RPCResultEvent(toWF(true), callId)
                    }
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

                  case ScopeCompletionReq(file: File, point: Int,
                    prefix: String, constructor: Boolean) => {
                    val p = pos(file, point)
                    val syms = scalaCompiler.askCompleteSymbolAt(p, prefix, constructor)
                    project ! RPCResultEvent(toWF(syms.map(toWF)), callId)
                  }

                  case TypeCompletionReq(file: File, point: Int, prefix: String) => {
                    val p = pos(file, point)
                    val members = scalaCompiler.askCompleteMemberAt(p, prefix)
                    project ! RPCResultEvent(toWF(members.map(toWF)), callId)
                  }

                  case ImportSuggestionsReq(_, _, _, _) => {
                    indexer ! rpcReq
                  }

                  case PublicSymbolSearchReq(_, _) => {
                    indexer ! rpcReq
                  }

                  case UsesOfSymAtPointReq(file: File, point: Int) => {
                    val p = pos(file, point)
                    val uses = scalaCompiler.askUsesOfSymAtPoint(p)
                    project ! RPCResultEvent(toWF(uses.map(toWF)), callId)
                  }

                  case PackageMemberCompletionReq(path: String, prefix: String) => {
                    val members = scalaCompiler.askCompletePackageMember(path, prefix)
                    project ! RPCResultEvent(toWF(members.map(toWF)), callId)
                  }

                  case InspectTypeReq(file: File, point: Int) => {
                    val p = pos(file, point)
                    val result = scalaCompiler.askInspectTypeAt(p) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case InspectTypeByIdReq(id: Int) => {
                    val result = scalaCompiler.askInspectTypeById(id) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case SymbolAtPointReq(file: File, point: Int) => {
                    val p = pos(file, point)
                    val result = scalaCompiler.askSymbolInfoAt(p) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case InspectPackageByPathReq(path: String) => {
                    val result = scalaCompiler.askPackageByPath(path) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case TypeAtPointReq(file: File, point: Int) => {
                    val p = pos(file, point)
                    val result = scalaCompiler.askTypeInfoAt(p) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case TypeByIdReq(id: Int) => {
                    val result = scalaCompiler.askTypeInfoById(id) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case TypeByNameReq(name: String) => {
                    val result = scalaCompiler.askTypeInfoByName(name) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case TypeByNameAtPointReq(name: String, file: File, point: Int) => {
                    val p = pos(file, point)
                    val result = scalaCompiler.askTypeInfoByNameAt(name, p) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case CallCompletionReq(id: Int) => {
                    val result = scalaCompiler.askCallCompletionInfoById(id) match {
                      case Some(info) => toWF(info)
                      case None => toWF(null)
                    }
                    project ! RPCResultEvent(result, callId)
                  }

                  case SymbolDesignationsReq(file: File, start: Int, end:Int, tpes: List[Symbol]) => {
		    val f = sourceFile(file)
		    val pos = new RangePosition(f, start, start, end)
                    val syms = scalaCompiler.askSymbolDesignationsInRegion(pos, tpes)
                    project ! RPCResultEvent(toWF(syms), callId)
                  }

                }
              }
            } catch {
              case e: Exception => {
                System.err.println("Error handling RPC: " + e + " :\n" +
                  e.getStackTraceString)
                project ! RPCErrorEvent(ErrExceptionInAnalyzer,
                  Some("Error occurred in Analyzer. Check the server log."), callId)
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

  def pos(file: File, offset: Int) = {
    val f = scalaCompiler.sourceFileForPath(file.getCanonicalPath())
    new OffsetPosition(f, offset)
  }

  def sourceFile(file: File) = {
    scalaCompiler.sourceFileForPath(file.getCanonicalPath())
  }

  override def finalize() {
    System.out.println("Finalizing Analyzer actor.")
  }

}

