package org.ensime.server

import scala.tools.nsc.interactive.{Global, 
  BuildManager, SimpleBuildManager, RefinedBuildManager}
import scala.tools.nsc.{Settings}
import scala.actors._  
import scala.actors.Actor._  
import scala.tools.nsc.io.{AbstractFile}
import scala.tools.nsc.util.{SourceFile, Position, OffsetPosition}
import scala.tools.nsc.util.NoPosition
import scala.tools.nsc.reporters.Reporter
import scala.collection.{Iterable, Map}
import scala.collection.mutable.{ HashMap, HashEntry, HashSet }
import scala.collection.mutable.{ ArrayBuffer, SynchronizedMap,LinkedHashMap }
import scala.collection.immutable.TreeSet
import scala.tools.nsc.ast._
import scala.tools.nsc.symtab.Types

import java.io.File

import org.ensime.util.RichFile._
import org.ensime.server.model._
import org.ensime.util._
import org.ensime.config.ProjectConfig
import org.ensime.server.model.SExpConversion._
import org.ensime.util.SExp._


case class BuilderShutdownEvent()

case class RebuildAllReq()
case class AddSourceFilesReq(files:Iterable[File])
case class RemoveSourceFilesReq(files:Iterable[File])
case class UpdateSourceFilesReq(files:Iterable[File])


class IncrementalBuilder(project:Project, config:ProjectConfig) extends Actor{

  class IncrementalBuildManager(settings: Settings, reporter:Reporter) extends RefinedBuildManager(settings) {
    class IncrementalGlobal(settings: Settings, reporter : Reporter) extends scala.tools.nsc.Global(settings, reporter)  {
      override def computeInternalPhases() {
	super.computeInternalPhases
	phasesSet += dependencyAnalysis
      }
      def newRun() = new Run()
    }
    override protected def newCompiler(settings: Settings) = new BuilderGlobal(settings, reporter) 
  }

  private val settings = new Settings(Console.println) 
  settings.processArguments(config.builderArgs, false)
  private val reporter = new PresentationReporter()
  private val bm:BuildManager = new IncrementalBuildManager(settings, reporter)

  import bm._

  def act(){

    loop {
      try{
	receive {
	  case BuilderShutdownEvent =>
	  {
	    exit('stop)
	  }
	  case RPCRequestEvent(req:Any, callId:Int) => {
	    try{
	      req match {

		case RebuildAllReq() =>
		{
		  project ! SendBackgroundMessageEvent("Building entire project. Please wait...")
		  val files = config.sourceFilenames.map(s => AbstractFile.getFile(s))
		  reporter.reset
		  bm.addSourceFiles(files)
		  bm.update(files, Set())
		  project ! SendBackgroundMessageEvent("Build complete.")
		  project ! RPCResultEvent(reporter.allNotes, callId)
		}
		case AddSourceFilesReq(files:Iterable[File]) =>
		{
		  val fileSet = files.map(AbstractFile.getFile(_)).toSet
		  bm.addSourceFiles(fileSet)
		  project ! RPCResultEvent(reporter.allNotes, callId)
		}
		case RemoveSourceFilesReq(files:Iterable[File]) =>
		{
		  val fileSet = files.map(AbstractFile.getFile(_)).toSet
		  project ! RPCResultEvent(TruthAtom(), callId)
		  reporter.reset
		  bm.removeFiles(fileSet)
		  project ! RPCResultEvent(reporter.allNotes, callId)
		}
		case UpdateSourceFilesReq(files:Iterable[File]) => 
		{
		  val fileSet = files.map(AbstractFile.getFile(_)).toSet
		  reporter.reset
		  bm.update(fileSet, Set())
		  project ! RPCResultEvent(reporter.allNotes, callId)
		}

	      }
	    }
	    catch{
	      case e:Exception =>
	      {

		System.err.println("Error handling RPC: " + 
		  e + " :\n" + 
		  e.getStackTraceString)
		project ! RPCErrorEvent("Error occurred in incremental builder. Check the server log.", callId)
	      }
	    }
	  }
	  case other => 
	  {
	    println("Incremental Builder: WTF, what's " + other)
	  }
	}

      }
      catch{
	case e:Exception =>
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

