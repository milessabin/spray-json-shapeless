package com.ensime.server

import scala.tools.nsc.interactive.{Global, CompilerControl}
import scala.tools.nsc.{Settings}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.actors._  
import scala.actors.Actor._  
import scala.tools.nsc.io.{AbstractFile}
import scala.tools.nsc.util.{SourceFile, Position, OffsetPosition}
import scala.tools.nsc.util.NoPosition
import scala.tools.nsc.reporters.Reporter
import scala.concurrent.SyncVar
import scala.collection.{Iterable, Map}
import scala.collection.mutable.{ HashMap, HashEntry, HashSet }
import scala.collection.mutable.{ ArrayBuffer, SynchronizedMap,LinkedHashMap }
import scala.collection.immutable.TreeSet
import java.io.File
import scala.tools.nsc.ast._
import com.ensime.util.RichFile._
import com.ensime.server.model._
import com.ensime.util._
import com.ensime.util.SExp._
import com.ensime.config.ProjectConfig
import com.ensime.server.model.SExpConversion._
import scala.tools.nsc.symtab.Types

case class FullTypeCheckCompleteEvent()
case class FullTypeCheckResultEvent(notes:List[Note])
case class QuickTypeCheckResultEvent(notes:List[Note])
case class CompilerShutdownEvent()

case class ReloadFileReq(file:File)
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



class Compiler(project:Project, config:ProjectConfig) extends Actor{

  val settings = new Settings(Console.println)
  settings.processArguments(config.compilerArgs, false)
  val reporter = new PresentationReporter()
  val global = new PresentationCompiler(settings, reporter, this, config)

  import global._

  def act(){
    global.newRunnerThread
    project ! SendBackgroundMessageEvent("Compiler is parsing sources...")
    global.blockingReloadAll
    project ! SendBackgroundMessageEvent("Compiler finished parsing!")
    loop {
      try{
	receive {
	  case CompilerShutdownEvent =>
	  {
	    global.clearTypeCache
	    global.askShutdown()
	    exit('stop)
	  }

	  case FullTypeCheckCompleteEvent() =>
	  {
	    project ! FullTypeCheckResultEvent(reporter.allNotes)
	  }

	  case RPCRequestEvent(req:Any, callId:Int) => {
	    try{
	      req match {

		case ReloadFileReq(file:File) =>
		{
		  val f = global.getSourceFile(file.getAbsolutePath())
		  global.blockingFullReload(f)
		  project ! RPCResultEvent(TruthAtom(), callId)
		  project ! QuickTypeCheckResultEvent(reporter.allNotes)
		}

		case RemoveFileReq(file:File) => 
		{
		  val f = global.getSourceFile(file.getAbsolutePath())
		  global.removeUnitOf(f)
		}

		case ScopeCompletionReq(file:File, point:Int, prefix:String, constructor:Boolean) => 
		{
		  val f = global.getSourceFile(file.getAbsolutePath())
		  val p = new OffsetPosition(f, point)
		  val syms = global.completeSymbolAt(p, prefix, constructor)
		  project ! RPCResultEvent(syms, callId)
		}

		case TypeCompletionReq(file:File, point:Int, prefix:String) => 
		{
		  val f = global.getSourceFile(file.getAbsolutePath())
		  val p = new OffsetPosition(f, point)
		  val members = global.completeMemberAt(p, prefix)
		  project ! RPCResultEvent(members, callId)
		}

		case InspectTypeReq(file:File, point:Int) =>
		{
		  val f = global.getSourceFile(file.getAbsolutePath())
		  val p = new OffsetPosition(f, point)
		  val inspectInfo = global.inspectTypeAt(p)
		  project ! RPCResultEvent(inspectInfo, callId)
		}

		case InspectTypeByIdReq(id:Int) =>
		{
		  val inspectInfo = global.typeById(id) match{
		    case Some(tpe) => global.inspectType(tpe)
		    case None => TypeInspectInfo.nullInfo
		  }
		  project ! RPCResultEvent(inspectInfo, callId)
		}

		case SymbolAtPointReq(file:File, point:Int) =>
		{
		  val f = global.getSourceFile(file.getAbsolutePath())
		  val p = new OffsetPosition(f, point)
		  val info = global.symbolInfoAt(p)
		  project ! RPCResultEvent(info, callId)
		}

		case InspectPackageByPathReq(path:String) =>
		{
		  val packageInfo = PackageInfo.fromPath(path)
		  project ! RPCResultEvent(packageInfo, callId)
		}

		case TypeAtPointReq(file:File, point:Int) =>
		{
		  val f = global.getSourceFile(file.getAbsolutePath())
		  val p = new OffsetPosition(f, point)
		  val typeInfo = global.getTypeInfoAt(p)
		  project ! RPCResultEvent(typeInfo, callId)
		}

		case TypeByIdReq(id:Int) =>
		{
		  val tpeInfo = global.typeById(id) match{
		    case Some(tpe) => TypeInfo(tpe)
		    case None => TypeInfo.nullInfo
		  }
		  project ! RPCResultEvent(tpeInfo, callId)
		}

		case CallCompletionReq(id:Int) =>
		{
		  val callInfo = global.typeById(id) match{
		    case Some(tpe) => CallCompletionInfo(tpe)
		    case None => CallCompletionInfo.nullInfo
		  }
		  project ! RPCResultEvent(callInfo, callId)
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
}

