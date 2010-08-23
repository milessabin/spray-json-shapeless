package org.ensime.server
import java.io.{ByteArrayOutputStream, File}
import java.util.Locale
import org.eclipse.jdt.core.compiler.{IProblem, CharOperation}
import org.eclipse.jdt.internal.compiler.{Compiler, ClassFile, CompilationResult, ICompilerRequestor, IErrorHandlingPolicy}
import org.eclipse.jdt.internal.compiler.batch.{CompilationUnit, FileSystem}
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileReader
import org.eclipse.jdt.internal.compiler.env.{NameEnvironmentAnswer, ICompilationUnit}
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions
import org.eclipse.jdt.internal.compiler.problem.DefaultProblemFactory
import org.ensime.config.ProjectConfig
import org.ensime.model._
import org.ensime.util._
import org.ensime.util.RichFile._
import scala.actors._
import scala.actors.Actor._
import scala.collection.{mutable, Iterable}
import scala.collection.JavaConversions._
import scala.tools.nsc.ast._

trait JavaCompiling { self: Analyzer =>

  private def classpath = config.compilerClasspathFilenames ++ ProjectConfig.javaBootJars.map(_.getPath)

  private val nameProvider = new FileSystem(classpath.toArray, Array(), "UTF-8"){

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
	compiledClasses(key) = reader

	// Remember package name
	val i = key.lastIndexOf(".")
	if(i > -1){
	  val packName = key.substring(0, i)
	  knownPackages += packName
	}
      }
    }

    override def findType(tpe:Array[Char], pkg:Array[Array[Char]]) = {
      findType(CharOperation.arrayConcat(pkg, tpe))
    }

    override def findType(compoundTypeName:Array[Array[Char]]) = {
      val key = CharOperation.toString(compoundTypeName)
      compiledClasses.get(key) match{
	case Some(binType) => {
	  new NameEnvironmentAnswer(binType, null)
	}
	case None => {
	  super.findType(compoundTypeName)
	}
      }
    }

    override def isPackage(parentPackageName:Array[Array[Char]], packageName:Array[Char]):Boolean = {
      if(super.isPackage(parentPackageName, packageName)){
	true
      }
      else{
	val key = CharOperation.toString(parentPackageName) + "." + CharOperation.charToString(packageName)
	knownPackages.contains(key)
      }
    }

  }

  private def errorPolicy = new IErrorHandlingPolicy{
    override def proceedOnErrors = true 
    override def stopOnFirstError = false 
  }

  private val settings:Map[String,Any] = Map(
    CompilerOptions.OPTION_Compliance -> "1.6",
    CompilerOptions.OPTION_Source -> "1.6",
    CompilerOptions.OPTION_TargetPlatform -> "1.6"
  )

  private val options = new CompilerOptions(settings)

  private val requestor = new ICompilerRequestor{

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

  private val problemFactory = new DefaultProblemFactory(Locale.ENGLISH)

  protected val javaCompiler = new Compiler(
    nameProvider, errorPolicy, 
    options, requestor, 
    problemFactory)

  protected val javaUnitForFile = new mutable.HashMap[String,ICompilationUnit]()

  protected def javaNotes():Iterable[Note] = {
    requestor.allNotes
  }

  protected def rebuildJavaUnits():Unit = {
    val defaultEncoding = "UTF-8"
    for(f <- config.sourceFilenames){
      if(f.endsWith(".java")){
	javaUnitForFile(f) = new CompilationUnit(null, f, defaultEncoding)
      }
    }
  }

}

