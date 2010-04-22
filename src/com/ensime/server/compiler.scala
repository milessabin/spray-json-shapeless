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
import com.ensime.server.model.SExpConversion._
import com.ensime.util.SExp._
import scala.tools.nsc.symtab.Types


case class FullTypeCheckCompleteEvent()
case class FullTypeCheckResultEvent(notes:List[Note])
case class QuickTypeCheckResultEvent(notes:List[Note])
case class ReloadFileEvent(file:File)
case class RemoveFileEvent(file:File)
case class ScopeCompletionEvent(file:File, point:Int, prefix:String, callId:Int)
case class TypeCompletionEvent(file:File, point:Int, prefix:String, callId:Int)
case class InspectTypeEvent(file:File, point:Int, callId:Int)
case class InspectTypeByIdEvent(id:Int, callId:Int)
case class InspectPackageByPathEvent(path:String, callId:Int)
case class TypeByIdEvent(id:Int, callId:Int)
case class TypeAtPointEvent(file:File, point:Int, callId:Int)
case class CompilerShutdownEvent()


class Compiler(project:Project, config:ProjectConfig) extends Actor{

  private val rootDir:File = new File(config.rootDir)

  private val classpathFiles:Set[String] = config.classpath.map{ s =>
    val f = new File(s)
    if(f.isAbsolute) f.getAbsolutePath
    else (new File(rootDir, s)).getAbsolutePath
  }.toSet

  private def isValidSourceFile(f:File):Boolean = {
    f.exists && !f.isHidden && (f.getName.endsWith(".scala") || f.getName.endsWith(".java"))
  }

  private def expandSource(srcList:Iterable[String]):Set[String] = {
    (for(
	s <- srcList;
	val f = new File(s);
	val files = if(f.isAbsolute) f.andTree else (new File(rootDir, s)).andTree;
	f <- files if isValidSourceFile(f)
      )
      yield{
	f.getAbsolutePath
      }).toSet
  }

  
  val includeSrcFiles = expandSource(config.srcList)
  val excludeSrcFiles = expandSource(config.excludeSrcList)
  val srcFiles = includeSrcFiles -- excludeSrcFiles

  val args = List(
    "-classpath", classpathFiles.mkString(":"),
    "-verbose",
    srcFiles.mkString(" ")
  )

  println("Compiler args: " + args)

  val settings = new Settings(Console.println)
  settings.processArguments(args, false)
  val reporter = new PresentationReporter()
  val global = new PresentationCompiler(settings, reporter, this, srcFiles)

  import global._


  def act() {
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

	  case ReloadFileEvent(file:File) => 
	  {
	    val f = global.getSourceFile(file.getAbsolutePath())
	    global.blockingFullReload(f)
	    project ! QuickTypeCheckResultEvent(reporter.allNotes)
	  }

	  case RemoveFileEvent(file:File) => 
	  {
	    val f = global.getSourceFile(file.getAbsolutePath())
	    global.removeUnitOf(f)
	  }

	  case ScopeCompletionEvent(file:File, point:Int, prefix:String, callId:Int) => 
	  {
	    val f = global.getSourceFile(file.getAbsolutePath())
	    val p = new OffsetPosition(f, point)
	    val syms = global.completeSymbolAt(p, prefix)
	    project ! RPCResultEvent(syms, callId)
	  }

	  case TypeCompletionEvent(file:File, point:Int, prefix:String, callId:Int) => 
	  {
	    val f = global.getSourceFile(file.getAbsolutePath())
	    val p = new OffsetPosition(f, point)
	    val members = global.completeMemberAt(p, prefix)
	    project ! RPCResultEvent(members, callId)
	  }

	  case InspectTypeEvent(file:File, point:Int, callId:Int) =>
	  {
	    val f = global.getSourceFile(file.getAbsolutePath())
	    val p = new OffsetPosition(f, point)
	    val inspectInfo = global.inspectTypeAt(p)
	    project ! RPCResultEvent(inspectInfo, callId)
	  }

	  case InspectTypeByIdEvent(id:Int, callId:Int) =>
	  {
	    val inspectInfo = global.typeById(id) match{
	      case Some(tpe) => global.inspectType(tpe)
	      case None => TypeInspectInfo.nullInfo
	    }
	    project ! RPCResultEvent(inspectInfo, callId)
	  }

	  case InspectPackageByPathEvent(path:String, callId:Int) =>
	  {
	    val packageInfo = PackageInfo.fromPath(path)
	    project ! RPCResultEvent(packageInfo, callId)
	  }

	  case TypeAtPointEvent(file:File, point:Int, callId:Int) =>
	  {
	    val f = global.getSourceFile(file.getAbsolutePath())
	    val p = new OffsetPosition(f, point)
	    val typeInfo = global.getTypeInfoAt(p)
	    project ! RPCResultEvent(typeInfo, callId)
	  }

	  case TypeByIdEvent(id:Int, callId:Int) =>
	  {
	    val tpeInfo = global.typeById(id) match{
	      case Some(tpe) => TypeInfo(tpe)
	      case None => TypeInfo.nullInfo
	    }
	    project ! RPCResultEvent(tpeInfo, callId)
	  }

	  case FullTypeCheckCompleteEvent() =>
	  {
	    project ! FullTypeCheckResultEvent(reporter.allNotes)
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

