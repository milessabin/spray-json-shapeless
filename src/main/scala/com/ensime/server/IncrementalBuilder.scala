package com.ensime.server

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

import com.ensime.util.RichFile._
import com.ensime.server.model._
import com.ensime.util._
import com.ensime.config.ProjectConfig
import com.ensime.server.model.SExpConversion._
import com.ensime.util.SExp._


case class BuilderShutdownEvent()

case class AddSourceFilesReq(files:Iterable[File])
case class RemoveSourceFilesReq(files:Iterable[File])
case class UpdateSourceFilesReq(files:Iterable[File])


class IncrementalBuilder(project:Project, config:ProjectConfig) extends Actor{
  private val settings = new Settings(Console.println) 
  settings.processArguments(config.builderArgs, false)
  private val bm:BuildManager = new RefinedBuildManager(settings)

  import bm._

  def act(){

    // Initialize
    val files = config.sourceFilenames.map(s => AbstractFile.getFile(s))
    bm.addSourceFiles(files)
    bm.update(files, Set())

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

		case AddSourceFilesReq(files:Iterable[File]) =>
		{
		  val fileSet = files.map(AbstractFile.getFile(_)).toSet
		  project ! RPCResultEvent(TruthAtom(), callId)
		  bm.addSourceFiles(fileSet)
		}
		case RemoveSourceFilesReq(files:Iterable[File]) =>
		{
		  val fileSet = files.map(AbstractFile.getFile(_)).toSet
		  project ! RPCResultEvent(TruthAtom(), callId)
		  bm.removeFiles(fileSet)
		}
		case UpdateSourceFilesReq(files:Iterable[File]) => 
		{
		  val fileSet = files.map(AbstractFile.getFile(_)).toSet
		  project ! RPCResultEvent(TruthAtom(), callId)
		  bm.update(fileSet, Set())
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

