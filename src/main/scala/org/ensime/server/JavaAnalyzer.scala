package org.ensime.server
import java.io.File
import java.util.Locale

import org.eclipse.jdt.internal.compiler.{Compiler, CompilationResult, ICompilerRequestor, IErrorHandlingPolicy}
import org.eclipse.jdt.internal.compiler.batch.{CompilationUnit, FileSystem}
import org.eclipse.jdt.internal.compiler.env.ICompilationUnit
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions
import org.eclipse.jdt.internal.compiler.problem.DefaultProblemFactory

import org.ensime.config.ProjectConfig
import org.ensime.model._
import org.ensime.protocol.ProtocolConversions
import org.ensime.util._
import org.ensime.util.RichFile._
import scala.actors._
import scala.actors.Actor._
import scala.collection.{mutable, Iterable}
import scala.collection.JavaConversions._
import scala.tools.nsc.ast._

class JavaAnalyzer(val project:Project, val protocol:ProtocolConversions, config:ProjectConfig) extends Actor {

  protected val classpath = config.compilerClasspathFilenames ++ ProjectConfig.javaBootJars.map(_.getPath)
  println("Java Classpath: " + classpath)

  protected val nameProvider = new FileSystem(classpath.toArray, Array(), "UTF-8")

  protected val errorPolicy = new IErrorHandlingPolicy{ 
    override def proceedOnErrors = true 
    override def stopOnFirstError = false 
  }

  private val settings:Map[String,Any] = Map(
    CompilerOptions.OPTION_Compliance -> "1.6",
    CompilerOptions.OPTION_Source -> "1.6",
    CompilerOptions.OPTION_TargetPlatform -> "1.6"
  )

  protected val options = new CompilerOptions(settings)

  protected val requestor = new ICompilerRequestor{
    override def acceptResult(result:CompilationResult) = { println("Java Compiler Result: " + result) }
  }

  protected val problemFactory = new DefaultProblemFactory(Locale.ENGLISH){

    private val notes = new mutable.HashMap[String, mutable.HashSet[Note]] {
      override def default(k : String) = { 
	val v = new mutable.HashSet[Note] ; put(k, v); v 
      }
    }

    def notesFor(file:String):NoteList = {
      NoteList(notes(file).toList)
    }

    def allNotes():NoteList = {
      NoteList(notes.flatMap{ e => e._2 }.toList)
    }
    
    def reset{
      notes.clear
    }

    override def createProblem(
      originatingFileName:Array[Char], 
      problemId:Int, 
      problemArguments:Array[String], 
      messageArguments:Array[String], 
      severity:Int, 
      startPosition:Int, 
      endPosition:Int, 
      lineNumber:Int,
      colNumber:Int) = { 

      val prob = super.createProblem(
	originatingFileName, problemId, problemArguments, 
	messageArguments, severity, startPosition, endPosition, 
	lineNumber, colNumber)

      notes(originatingFileName.mkString) += Note(prob)
      prob
    }
  }

  val compiler = new Compiler(
    nameProvider, errorPolicy, 
    options, requestor, 
    problemFactory)

  import protocol._

  val unitForFile = new mutable.HashMap[String,ICompilationUnit]()

  def rebuildUnits():Unit = {
    val defaultEncoding = "UTF-8"
    for(f <- config.sourceFilenames){
      if(f.endsWith(".java")){
	unitForFile(f) = new CompilationUnit(null, f, defaultEncoding)
      }
    }
  }

  def act(){
    rebuildUnits
    println("Adding java units: " + unitForFile.values.toList)
    compiler.compile(unitForFile.values.toArray)
    project ! SendBackgroundMessageEvent("Java Analyzer is working. Please wait...")
    loop {
      try{
	receive {
	  case AnalyzerShutdownEvent() =>
	  {
	    compiler.reset
	    exit('stop)
	  }
	  case RPCCommandEvent(req:Any) => {
	    try{
	      req match {
		case ReloadAllReq() =>
		{
		  problemFactory.reset
		  compiler.reset
		  println("JAVA: is it sync?")
		  compiler.compile(unitForFile.values.toArray)
		  println("JAVA: done compiling")
		  project ! FullTypeCheckResultEvent(problemFactory.allNotes)
		}
		case ReloadFileReq(file:File) =>
		{
		  problemFactory.reset
		  val units = Array(unitForFile(file.getCanonicalPath))
		  println("JAVA: is it sync?")
		  compiler.compile(units)
		  println("JAVA: done compiling")
		  project ! QuickTypeCheckResultEvent(problemFactory.allNotes)
		}
	      }
	    }
	    catch{
	      case e:Exception =>
	      {
		System.err.println("Error handling RPC Command: " + e + " :\n" + e.getStackTraceString)
		project ! RPCCommandErrorEvent("Error occurred in Java Analyzer. Check the server log.")
	      }
	    }
	  }
	  case RPCRequestEvent(req:Any, callId:Int) => {
	    try{
	      req match {
		case _ => {}
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
	    println("Java Analyzer: WTF, what's " + other)
	  }
	}

      }
      catch{
	case e:Exception =>
	{
	  System.err.println("Error at Java Analyzer message loop: " + e + " :\n" + e.getStackTraceString)
	}
      }
    }
  }

  override def finalize() {
    System.out.println("Finalizing compilation actor.")
  }

}

