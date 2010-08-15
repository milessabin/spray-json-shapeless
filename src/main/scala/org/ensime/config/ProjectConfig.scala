package org.ensime.config
import java.io.File
import org.ensime.util._
import org.ensime.util.FileUtils._
import org.ensime.util.RichFile._
import org.ensime.util.SExp._
import scala.actors._
import scala.actors.Actor._
import scala.collection.mutable



object ProjectConfig{

  /**
  * Create a ProjectConfig instance from the given
  * SExp property list.
  */
  def apply(config:SExpList) = {
    import ExternalConfigInterface._

    val m = config.toKeywordMap

    val rootDir = m.get(key(":root-dir")) match{
      case Some(StringAtom(str)) => new File(str)
      case _ => new File(".")
    }

    val sourceRoots = new mutable.HashSet[File]
    val runtimeDeps = new mutable.HashSet[File]
    val compileDeps = new mutable.HashSet[File]
    val classDirs = new mutable.HashSet[File]
    var target:Option[File] = None

    m.get(key(":use-sbt")) match{
      case Some(TruthAtom()) => {
	val ext = getSbtConfig(rootDir)
	sourceRoots ++= ext.sourceRoots
	runtimeDeps ++= ext.runtimeDepJars
	compileDeps ++= ext.compileDepJars
	compileDeps ++= ext.testDepJars
	target = ext.target
      }
      case _ => 
    }

    m.get(key(":use-maven")) match{
      case Some(TruthAtom()) => {
	val ext = getMavenConfig(rootDir)
	sourceRoots ++= ext.sourceRoots
	runtimeDeps ++= ext.runtimeDepJars
	compileDeps ++= ext.compileDepJars
	compileDeps ++= ext.testDepJars
	target = ext.target
      }
      case _ => 
    }

    m.get(key(":use-ivy")) match{
      case Some(TruthAtom()) => {
	val rConf = m.get(key(":ivy-runtime-conf")).map(_.toString)
	val cConf = m.get(key(":ivy-compile-conf")).map(_.toString)
	val tConf = m.get(key(":ivy-test-conf")).map(_.toString)
	val ext = getIvyConfig(rootDir, rConf, cConf, tConf)
	sourceRoots ++= ext.sourceRoots
	runtimeDeps ++= ext.runtimeDepJars
	compileDeps ++= ext.compileDepJars
	compileDeps ++= ext.testDepJars
	target = ext.target
      }
      case _ => 
    }
    
    m.get(key(":dependency-jars")) match{
      case Some(SExpList(items)) => {
	val jars = makeFiles(items.map(_.toString), rootDir)
	compileDeps ++= expandRecursively(rootDir, jars, isValidJar _)
	runtimeDeps ++= expandRecursively(rootDir, jars, isValidJar _)
      }
      case _ => 
    }

    m.get(key(":runtime-dependency-jars")) match{
      case Some(SExpList(items)) => {
	val jars = makeFiles(items.map(_.toString), rootDir)
	runtimeDeps ++= expandRecursively(rootDir, jars, isValidJar _)
      }
      case _ => 
    }

    m.get(key(":compile-dependency-jars")) match{
      case Some(SExpList(items)) => {
	val jars = makeFiles(items.map(_.toString), rootDir)
	compileDeps ++= expandRecursively(rootDir, jars, isValidJar _)
      }
      case _ => 
    }

    m.get(key(":dependency-dirs")) match{
      case Some(SExpList(items)) => {
	val dirs = makeDirs(items.map(_.toString), rootDir)
	classDirs ++= expand(rootDir,dirs,isValidClassDir _)
      }
      case _ => 
    }

    m.get(key(":sources")) match{
      case Some(SExpList(items)) => {
	val dirs = makeDirs(items.map(_.toString), rootDir)
	sourceRoots ++= dirs
      }
      case _ => 
    }
    val sourceFiles = expandRecursively(rootDir,sourceRoots,isValidSourceFile _)

    val debugClass = m.get(key(":debug-class")).map(_.toString)
    val debugArgs = m.get(key(":debug-args")) match{
      case Some(SExpList(items)) => {
	items.map(_.toString)
      }
      case _ => List()
    }

    new ProjectConfig(rootDir, sourceFiles, 
      sourceRoots, runtimeDeps, compileDeps, 
      classDirs, target, debugClass, debugArgs)
    
  }


  def nullConfig = new ProjectConfig(new File("."), List(), List(), 
    List(), List(), List(), None, None, List())

}


class ReplConfig(val classpath:String){}

class DebugConfig(val classpath:String, val sourcepath:String){}

class ProjectConfig(
  val root:File,
  val sources:Iterable[File],
  val sourceRoots:Iterable[File],
  val runtimeDeps:Iterable[File],
  val compileDeps:Iterable[File],
  val classDirs:Iterable[File],
  val target:Option[File],
  val debugClass:Option[String],
  val debugArgs:Iterable[String]){

  def compilerClasspathFilenames:Set[String] = {
    val allFiles = compileDeps ++ classDirs
    allFiles.map(_.getAbsolutePath).toSet
  }

  def sourceFilenames:Set[String] = {
    sources.map(_.getAbsolutePath).toSet
  }

  def compilerArgs = List(
    "-classpath", compilerClasspathFilenames.mkString(File.pathSeparator),
    "-verbose",
    sourceFilenames.mkString(" ")
  )

  def builderArgs = List(
    "-classpath", compilerClasspathFilenames.mkString(File.pathSeparator),
    "-verbose",
    "-d", target.getOrElse(new File(root,"classes")).getAbsolutePath,
    "-Ybuildmanagerdebug",
    sourceFilenames.mkString(" ")
  )

  def runtimeClasspath = {
    val allFiles = runtimeDeps ++ classDirs ++ target
    val paths = allFiles.map(_.getAbsolutePath).toSet
    paths.mkString(File.pathSeparator)
  }

  def replClasspath = runtimeClasspath

  def debugClasspath = runtimeClasspath

  def debugArgString = debugArgs.map(_.toString).mkString(" ")

  def debugSourcepath = {
    sourceRoots.map(_.getAbsolutePath).toSet.mkString(File.pathSeparator)
  }

  def replConfig = new ReplConfig(replClasspath)

  def debugConfig = new DebugConfig(debugClasspath, debugSourcepath)

}



