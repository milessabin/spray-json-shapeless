package org.ensime.server

import scala.collection.mutable.ArrayBuffer
import java.io.ByteArrayOutputStream
import java.util.Locale
import org.eclipse.jdt.core.compiler.CharOperation
import org.eclipse.jdt.internal.compiler.{ Compiler, ClassFile, CompilationResult, ICompilerRequestor, DefaultErrorHandlingPolicies }
import org.eclipse.jdt.internal.compiler.batch.{ CompilationUnit, FileSystem }
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileReader
import org.eclipse.jdt.internal.compiler.env.{ NameEnvironmentAnswer, ICompilationUnit }
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions
import org.eclipse.jdt.internal.compiler.problem.DefaultProblemFactory
import org.ensime.config.ProjectConfig
import org.ensime.model._
import org.ensime.util._
import scala.actors._
import scala.collection.{ mutable, Iterable }
import scala.collection.JavaConversions._

class JavaCompiler(
    config: ProjectConfig,
    val reportHandler: ReportHandler,
    val indexer: Actor) {

  private val javaUnitForFile = new mutable.HashMap[String, ICompilationUnit]()

  val defaultEncoding = "UTF-8"
  for (f <- config.sourceFilenames) {
    if (f.endsWith(".java")) {
      javaUnitForFile(f) = new CompilationUnit(null, f, defaultEncoding)
    }
  }

  class NameProvider(classpath: Array[String]) extends FileSystem(
    classpath.toArray, Array(), "UTF-8") {

    private val compiledClasses = new mutable.HashMap[String, ClassFileReader]()
    private val knownPackages = new mutable.HashSet[String]()

    def addClassFiles(classFiles: Iterable[ClassFile]) {
      val infos = new ArrayBuffer[SymbolSearchResult]
      for (cf <- classFiles) {
        val byteStream = new ByteArrayOutputStream()
        byteStream.write(cf.header, 0, cf.headerOffset)
        byteStream.write(cf.contents, 0, cf.contentsOffset)
        val bytes = byteStream.toByteArray
        val reader = new ClassFileReader(bytes, cf.fileName)
        val cn = cf.getCompoundName
        val key = CharOperation.toString(cn)
        compiledClasses(key) = reader

        // Add type to the indexer
        if (org.ensime.server.Indexer.isValidType(key)) {
          val localName = if (cn.length > 0) {
            CharOperation.charToString(cn(cn.length - 1))
          } else "NA"
          val pos = Some((CharOperation.charToString(reader.getFileName), 0))
          infos += TypeSearchResult(
            key,
            localName,
            'class,
            pos)
        }

        // Remember package names
        var i = key.indexOf(".")
        while (i > -1) {
          val packName = key.substring(0, i)
          knownPackages += packName
          i = key.indexOf(".", i + 1)
        }
      }
      indexer ! AddSymbolsReq(infos)
    }

    override def findType(tpe: Array[Char], pkg: Array[Array[Char]]) = {
      findType(CharOperation.arrayConcat(pkg, tpe))
    }

    override def findType(compoundTypeName: Array[Array[Char]]) = {
      val key = CharOperation.toString(compoundTypeName)
      compiledClasses.get(key) match {
        case Some(binType) =>
          new NameEnvironmentAnswer(binType, null)
        case None =>
          super.findType(compoundTypeName)
      }
    }

    override def isPackage(parentPackageName: Array[Array[Char]], packageName: Array[Char]): Boolean = {
      if (super.isPackage(parentPackageName, packageName)) {
        true
      } else {
        val key = CharOperation.toString(parentPackageName) + "." + CharOperation.charToString(packageName)
        knownPackages.contains(key)
      }
    }

  }

  private def classpath = config.compilerClasspathFilenames ++ ProjectConfig.javaBootJars.map(_.getPath) ++
    config.target.map(t => Set[String](t.getAbsolutePath)).getOrElse(Set[String]())

  private val nameProvider = new NameProvider(classpath.toArray)

  private val errorPolicy = DefaultErrorHandlingPolicies.proceedWithAllProblems()

  private val JavaVersion = config.javaCompilerVersion.getOrElse("1.6")

  private val extraOptions: Map[String, String] = {
    var m = Map[String, String]()
    config.javaCompilerArgs.sliding(2, 2).foreach {
      case key :: value :: rest => m += (key -> value)
      case _ =>
    }
    m
  }

  private val options = new CompilerOptions(Map(
    CompilerOptions.OPTION_Compliance -> JavaVersion,
    CompilerOptions.OPTION_Source -> JavaVersion,
    CompilerOptions.OPTION_TargetPlatform -> JavaVersion) ++ extraOptions)

  class Requester(nameProvider: NameProvider) extends ICompilerRequestor {
    def allNotes(): List[Note] = problems
    private var problems: List[Note] = List()
    override def acceptResult(result: CompilationResult) = {
      problems = List()
      if (result.hasProblems) {
        problems = result.getProblems.map(Note.apply).toList
        reportHandler.reportJavaNotes(problems)
      }
      nameProvider.addClassFiles(result.getClassFiles)
    }
  }

  private val requestor = new Requester(nameProvider)

  private val problemFactory = new DefaultProblemFactory(Locale.ENGLISH)

  protected val compiler = new Compiler(
    nameProvider, errorPolicy,
    options, requestor,
    problemFactory) {

    override def finalize() {
      System.out.println("Finalizing Java Compiler.")
    }

  }

  def compileAll() = {
    val units = javaUnitForFile.values
    if (units.nonEmpty) {
      try {
        compiler.compile(units.toArray)
      } catch {
        case e: Exception =>
          System.err.println("Java compilation failed.")
          e.printStackTrace(System.err)
      }
    }
  }

  def addFile(f: SourceFileInfo) = {
    val path = f.file.getCanonicalPath
    if (path.endsWith(".java")) {
      val contents = f.contents match {
        case None => null
        case Some(s) => s.toCharArray
      }
      javaUnitForFile(path) = new CompilationUnit(contents, path, defaultEncoding)
    }
  }

  def compileFiles(files: List[SourceFileInfo]) = {
    reportHandler.clearAllJavaNotes()
    files.foreach(addFile)
    try {
      val units = files.flatMap(f => javaUnitForFile.get(f.file.getCanonicalPath))
      compiler.compile(units.toArray)
    } catch {
      case e: Exception =>
        System.err.println("Java compilation failed.")
        e.printStackTrace(System.err)
    }
  }

  def allNotes: Iterable[Note] = {
    requestor.allNotes()
  }

  def reset() {
    compiler.reset()
    reportHandler.clearAllJavaNotes()
  }

  def shutdown() {
    compiler.reset()
    javaUnitForFile.clear()
  }

}

