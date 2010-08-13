package org.ensime.server
import java.io.File
import org.ensime.config.ProjectConfig
import org.ensime.model._
import org.ensime.protocol.ProtocolConversions
import org.ensime.util._
import org.ensime.util.RichFile._
import scala.actors._
import scala.actors.Actor._
import scala.collection.{Iterable, Map}
import scala.collection.immutable.TreeSet
import scala.collection.mutable.{ ArrayBuffer, SynchronizedMap, LinkedHashMap, HashMap, HashEntry, HashSet }
import scala.concurrent.SyncVar
import scala.tools.nsc.{Settings}
import scala.tools.nsc.ast._

import scala.tools.nsc.interactive.{Global, CompilerControl}
import scala.tools.nsc.io.{AbstractFile}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.symtab.Types
import scala.tools.nsc.util.{NoPosition, SourceFile, Position, OffsetPosition}


case class FullTypeCheckCompleteEvent()
case class FullTypeCheckResultEvent(notes:NoteList)
case class QuickTypeCheckResultEvent(notes:NoteList)
case class CompilerReadyEvent()
case class CompilerShutdownEvent()

case class ReloadFileReq(file:File)
case class ReloadAllReq()
case class RemoveFileReq(file:File)
case class RPCRequestEvent(req:Any, callId:Int)
case class ScopeCompletionReq(file:File, point:Int, prefix:String, constructor:Boolean)
case class TypeCompletionReq(file:File, point:Int, prefix:String)
case class SymbolAtPointReq(file:File, point:Int)
case class InspectTypeReq(file:File, point:Int)
case class InspectTypeByIdReq(id:Int)
case class InspectPackageByPathReq(path:String)
case class TypeByIdReq(id:Int)
case class CallCompletionReq(id:Int)
case class TypeAtPointReq(file:File, point:Int)


class Compiler(val project:Project, val protocol:ProtocolConversions, config:ProjectConfig) extends Actor with RefactoringController{
  protected val settings = new Settings(Console.println)
  settings.processArguments(config.compilerArgs, false)
  protected val reporter = new PresentationReporter()
  protected val cc:RichCompilerControl = new RichPresentationCompiler(settings, reporter, this, config)
  protected var awaitingInitialCompile = true

  import cc._
  import protocol._

  def act(){
    cc.askNewRunnerThread
    project ! SendBackgroundMessageEvent("Compiler is loading sources. Please wait...")
    cc.askReloadAllFiles
    loop {
      try{
	receive {
	  case CompilerShutdownEvent =>
	  {
	    cc.askClearTypeCache
	    cc.askShutdown()
	    exit('stop)
	  }

	  case FullTypeCheckCompleteEvent() =>
	  {
	    if(awaitingInitialCompile){
	      project ! CompilerReadyEvent()
	      awaitingInitialCompile = false
	    }
	    project ! FullTypeCheckResultEvent(reporter.allNotes)
	  }

	  case RPCRequestEvent(req:Any, callId:Int) => {
	    try{
	      req match {

		case req:RefactorPrepReq =>
		{
		  handleRefactorRequest(req, callId)
		}

		case req:RefactorExecReq =>
		{
		  handleRefactorExec(req, callId)
		}

		case req:RefactorCancelReq =>
		{
		  handleRefactorCancel(req, callId)
		}

		case ReloadAllReq() =>
		{
		  cc.askReloadAllFiles()
		  project ! RPCResultEvent(toWF(true), callId)
		}

		case ReloadFileReq(file:File) =>
		{
		  val f = cc.sourceFileForPath(file.getAbsolutePath())
		  cc.askReloadFile(f)
		  project ! RPCResultEvent(toWF(true), callId)
		  project ! QuickTypeCheckResultEvent(reporter.allNotes)
		}

		case RemoveFileReq(file:File) => 
		{
		  val f = cc.sourceFileForPath(file.getAbsolutePath())
		  cc.removeUnitOf(f)
		}

		case ScopeCompletionReq(file:File, point:Int, prefix:String, constructor:Boolean) => 
		{
		  val f = cc.sourceFileForPath(file.getAbsolutePath())
		  val p = new OffsetPosition(f, point)
		  val syms = cc.askCompleteSymbolAt(p, prefix, constructor)
		  project ! RPCResultEvent(toWF(syms.map(toWF)), callId)
		}

		case TypeCompletionReq(file:File, point:Int, prefix:String) => 
		{
		  val f = cc.sourceFileForPath(file.getAbsolutePath())
		  val p = new OffsetPosition(f, point)
		  val members = cc.askCompleteMemberAt(p, prefix)
		  project ! RPCResultEvent(toWF(members.map(toWF)), callId)
		}

		case InspectTypeReq(file:File, point:Int) =>
		{
		  val f = cc.sourceFileForPath(file.getAbsolutePath())
		  val p = new OffsetPosition(f, point)
		  val inspectInfo = cc.askInspectTypeAt(p)
		  project ! RPCResultEvent(toWF(inspectInfo), callId)
		}

		case InspectTypeByIdReq(id:Int) =>
		{
		  val inspectInfo = cc.askInspectTypeById(id)
		  project ! RPCResultEvent(toWF(inspectInfo), callId)
		}

		case SymbolAtPointReq(file:File, point:Int) =>
		{
		  val f = cc.sourceFileForPath(file.getAbsolutePath())
		  val p = new OffsetPosition(f, point)
		  val info = cc.askSymbolInfoAt(p)
		  project ! RPCResultEvent(toWF(info), callId)
		}

		case InspectPackageByPathReq(path:String) =>
		{
		  val packageInfo = cc.askPackageByPath(path)
		  project ! RPCResultEvent(toWF(packageInfo), callId)
		}

		case TypeAtPointReq(file:File, point:Int) =>
		{
		  val f = cc.sourceFileForPath(file.getAbsolutePath())
		  val p = new OffsetPosition(f, point)
		  val typeInfo = cc.askTypeInfoAt(p)
		  project ! RPCResultEvent(toWF(typeInfo), callId)
		}

		case TypeByIdReq(id:Int) =>
		{
		  val tpeInfo = cc.askTypeInfoById(id)
		  project ! RPCResultEvent(toWF(tpeInfo), callId)
		}

		case CallCompletionReq(id:Int) =>
		{
		  val callInfo = cc.askCallCompletionInfoById(id)
		  project ! RPCResultEvent(toWF(callInfo), callId)
		}
	      }
	    }
	    catch{
	      case e:Exception =>
	      {
		System.err.println("Error handling RPC: " + e + " :\n" + e.getStackTraceString)
		project ! RPCErrorEvent("Error occurred in compiler. Check the server log.", callId)
	      }
	    }
	  }
	  case other => 
	  {
	    println("Compiler: WTF, what's " + other)
	  }
	}

      }
      catch{
	case e:Exception =>
	{
	  System.err.println("Error at Compiler message loop: " + e + " :\n" + e.getStackTraceString)
	}
      }
    }
  }

  override def finalize() {
    System.out.println("Finalizing compilation actor.")
  }

}

