/**
*  Copyright (C) 2010 Aemon Cannon
*
*  This program is free software; you can redistribute it and/or
*  modify it under the terms of the GNU General Public License as
*  published by the Free Software Foundation; either version 2 of
*  the License, or (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public
*  License along with this program; if not, write to the Free
*  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
*  MA 02111-1307, USA.
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
                    val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
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
  val f = scalaCompiler.sourceFileForPath(file.getAbsolutePath())
  new OffsetPosition(f, offset)
}

override def finalize() {
  System.out.println("Finalizing Analyzer actor.")
}

}

