package org.ensime.server
import java.io.File
import java.io.ByteArrayOutputStream
import java.util.Locale

import org.eclipse.jdt.internal.compiler.{Compiler, ClassFile, CompilationResult, ICompilerRequestor, IErrorHandlingPolicy}
import org.eclipse.jdt.internal.compiler.batch.{CompilationUnit, FileSystem}
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileReader
import org.eclipse.jdt.internal.compiler.env.{NameEnvironmentAnswer, ICompilationUnit}
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions
import org.eclipse.jdt.internal.compiler.problem.DefaultProblemFactory
import org.eclipse.jdt.core.compiler.CharOperation;
import org.eclipse.jdt.core.compiler.IProblem

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



  protected val nameProvider = new FileSystem(classpath.toArray, Array(), "UTF-8"){

    private val compiledClasses = new mutable.HashMap[String, ClassFileReader]()
    private val knownPackages = new mutable.HashSet[String]()

    def addClassFiles(classFiles:Iterable[ClassFile]) {
      for(cf <- classFiles){
	val byteStream = new ByteArrayOutputStream()
	byteStream.write(cf.header, 0, cf.headerOffset)
	byteStream.write(cf.contents, 0, cf.contentsOffset)
	val bytes = byteStream.toByteArray
	val reader = new ClassFileReader(bytes, cf.fileName)
	val key = CharOperation.toString(cf.getCompoundName)
	println("JAVA: Saving type " + key)
	compiledClasses(key) = reader

	// Remember package name
	val i = key.lastIndexOf(".")
	if(i > -1){
	  val packName = key.substring(0, i)
	  println("JAVA: Remembering package " + packName)
	  knownPackages += packName
	}
      }
    }

    override def findType(tpe:Array[Char], pkg:Array[Array[Char]]) = {
      findType(CharOperation.arrayConcat(pkg, tpe))
    }

    override def findType(compoundTypeName:Array[Array[Char]]) = {
      val key = CharOperation.toString(compoundTypeName)
      println("JAVA: Looking for type " + key)
      compiledClasses.get(key) match{
	case Some(binType) => {
	  println("  Found it!")	  
	  new NameEnvironmentAnswer(binType, null)
	}
	case None => {
	  val result = super.findType(compoundTypeName)
	  if(result != null){
	    println("  Super found it!")
	  }
	  else{
	    println("  Super failed")
	  }
	  result
	}
      }
    }

    override def isPackage(parentPackageName:Array[Array[Char]], packageName:Array[Char]):Boolean = {
      if(super.isPackage(parentPackageName, packageName)){
	true
      }
      else{
	val key = CharOperation.toString(parentPackageName) + "." + CharOperation.charToString(packageName)
	println("JAVA: Looking for package " + key)
	knownPackages.contains(key)
      }
    }

  }

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

    def allNotes():Iterable[Note] = {
      problems.map(Note.apply)
    }
    
    private var problems:Iterable[IProblem] = List()

    override def acceptResult(result:CompilationResult) = { 
      problems = List()
      if(result.hasProblems){
	problems = result.getProblems.toList
      }
      nameProvider.addClassFiles(result.getClassFiles)
    }

  }

  protected val problemFactory = new DefaultProblemFactory(Locale.ENGLISH){

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
    compiler.compile(unitForFile.values.toArray)
    project ! SendBackgroundMessageEvent("Initializing Java Analyzer...")
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
		  compiler.reset
		  compiler.compile(unitForFile.values.toArray)
		  val result = NoteList('java, true, requestor.allNotes)
		  project ! TypeCheckResultEvent(result)
		}
		case ReloadFileReq(file:File) =>
		{
		  for( u <- unitForFile.get(file.getCanonicalPath)){
		    compiler.compile(Array(u))
		    val result = NoteList('java, false, requestor.allNotes)
		    project ! TypeCheckResultEvent(result)
		  }
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

