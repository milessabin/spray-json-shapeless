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

    val rootDir:CanonFile = m.get(key(":root-dir")) match{
      case Some(StringAtom(str)) => new File(str)
      case _ => new File(".")
    }

    val sourceRoots = new mutable.HashSet[CanonFile]
    val runtimeDeps = new mutable.HashSet[CanonFile]
    val compileDeps = new mutable.HashSet[CanonFile]
    val classDirs = new mutable.HashSet[CanonFile]
    var target:Option[CanonFile] = None

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
	val file = m.get(key(":ivy-file")).map(s => new File(s.toString))
	val ext = getIvyConfig(rootDir, file, rConf, cConf, tConf)
	sourceRoots ++= ext.sourceRoots
	runtimeDeps ++= ext.runtimeDepJars
	compileDeps ++= ext.compileDepJars
	compileDeps ++= ext.testDepJars
	target = ext.target
      }
      case _ => 
    }

    m.get(key(":runtime-jars")) match{
      case Some(SExpList(items)) => {
	val jarsAndDirs = maybeFiles(items.map(_.toString), rootDir)
	runtimeDeps ++= expandRecursively(rootDir, jarsAndDirs, isValidJar _)
      }
      case _ => 
    }

    m.get(key(":exclude-runtime-jars")) match{
      case Some(SExpList(items)) => {
	val jarsAndDirs = maybeFiles(items.map(_.toString), rootDir)
	runtimeDeps --= expandRecursively(rootDir, jarsAndDirs, isValidJar _)
      }
      case _ => 
    }

    m.get(key(":compile-jars")) match{
      case Some(SExpList(items)) => {
	val jarsAndDirs = maybeFiles(items.map(_.toString), rootDir)
	compileDeps ++= expandRecursively(rootDir, jarsAndDirs, isValidJar _)
      }
      case _ => 
    }

    m.get(key(":exclude-compile-jars")) match{
      case Some(SExpList(items)) => {
	val jarsAndDirs = maybeFiles(items.map(_.toString), rootDir)
	compileDeps --= expandRecursively(rootDir, jarsAndDirs, isValidJar _)
      }
      case _ => 
    }

    m.get(key(":class-dirs")) match{
      case Some(SExpList(items)) => {
	val dirs = maybeDirs(items.map(_.toString), rootDir)
	classDirs ++= expand(rootDir,dirs,isValidClassDir _)
      }
      case _ => 
    }

    m.get(key(":sources")) match{
      case Some(SExpList(items)) => {
	val dirs = maybeDirs(items.map(_.toString), rootDir)
	sourceRoots ++= dirs
      }
      case _ => 
    }

    m.get(key(":target")) match{
      case Some(StringAtom(targetDir)) => {
	target = target.orElse(maybeDir(targetDir, rootDir))
      }
      case _ => 
    }




    // Provide some reasonable defaults..

    target = verifyTargetDir(rootDir, target, new File(rootDir, "target/classes"))
    println("Using target directory: " + target.getOrElse("ERROR"))

    if(sourceRoots.isEmpty){
      val f = new File("src")
      if(f.exists && f.isDirectory){
	println("Using default source root, 'src'.")
	sourceRoots += f
      }
    }


    new ProjectConfig(
      rootDir,sourceRoots,runtimeDeps,
      compileDeps,classDirs,target)
    
  }

  // If given target directory is not valid, use the default,
  // creating if necessary.
  def verifyTargetDir(rootDir:File, target:Option[File], defaultTarget:File):Option[CanonFile] = {
    val targetDir = target match{
      case Some(f:File) => {
	if(f.exists && f.isDirectory) { f } else{ defaultTarget }
      }
      case None => defaultTarget
    }
    if(targetDir.exists){
      Some(targetDir)
    }
    else{
      try{
	if(targetDir.mkdirs) Some(targetDir)
	else None
      }
      catch{
	case e => None
      }
    }
  }


  def nullConfig = new ProjectConfig(new File("."), List(), 
    List(), List(), List(), None)

}


class ReplConfig(val classpath:String){}

class DebugConfig(val classpath:String, val sourcepath:String){}

class ProjectConfig(
  val root:CanonFile,
  val sourceRoots:Iterable[CanonFile],
  val runtimeDeps:Iterable[CanonFile],
  val compileDeps:Iterable[CanonFile],
  val classDirs:Iterable[CanonFile],
  val target:Option[CanonFile]){

  def compilerClasspathFilenames:Set[String] = {
    (compileDeps ++ classDirs).map(_.getPath).toSet
  }

  def sources:Set[CanonFile] = {
    expandRecursively(root,sourceRoots,isValidSourceFile _).toSet
  }

  def sourceFilenames:Set[String] = {
    sources.map(_.getPath).toSet
  }

  def compilerArgs = List(
    "-classpath", compilerClasspath,
    "-verbose",
    sourceFilenames.mkString(" ")
  )

  def builderArgs = List(
    "-classpath", compilerClasspath,
    "-verbose",
    "-d", target.getOrElse(new File(root,"classes")).getPath,
    "-Ybuildmanagerdebug",
    sourceFilenames.mkString(" ")
  )

  def compilerClasspath:String = {
    compilerClasspathFilenames.mkString(File.pathSeparator)
  }

  def runtimeClasspath:String = {
    val allFiles = compileDeps ++ runtimeDeps ++ classDirs ++ target
    val paths = allFiles.map(_.getPath).toSet
    paths.mkString(File.pathSeparator)
  }

  def replClasspath = runtimeClasspath

  def debugClasspath = runtimeClasspath

  def debugSourcepath = {
    sourceRoots.map(_.getPath).toSet.mkString(File.pathSeparator)
  }

  def replConfig = new ReplConfig(replClasspath)

  def debugConfig = new DebugConfig(debugClasspath, debugSourcepath)

}



