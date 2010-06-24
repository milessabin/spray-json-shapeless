package com.ensime.config
import java.io.File
import org.apache.ivy.{core, plugins, util, Ivy}
import core.cache.ArtifactOrigin
import core.IvyPatternHelper
import core.cache.DefaultRepositoryCacheManager
import com.ensime.util.FileUtils._
import org.apache.ivy.ant.IvyCacheTask
import org.apache.ivy.core.report.ArtifactDownloadReport;
import scala.collection.JavaConversions._
import org.apache.tools.ant._
import org.apache.maven.artifact.ant._
import com.ensime.util.FileUtils._

object ExternalConfigInterface {

  private def newConsoleLogger = {
    val consoleLogger:DefaultLogger = new DefaultLogger()
    consoleLogger.setErrorPrintStream(System.err)
    consoleLogger.setOutputPrintStream(System.out)
    consoleLogger.setMessageOutputLevel(Project.MSG_INFO)
    consoleLogger
  }



  def getMavenConfig(baseDir:File, buildScopes:Option[String]):(Iterable[File], Iterable[File], Option[File]) = {

    val srcDirs = List("src/main/scala", "src/main/java").map(new File(_))

    System.out.println("Resolving Maven dependencies...")
    val project = new Project()
    project.addBuildListener(newConsoleLogger)
    project.setBaseDir(baseDir)
    project.init()
    val target = new Target()
    target.setName("ResolveDependencies")
    target.setProject(project)

    val pom = new Pom() 
    pom.setFile(new File(baseDir, "pom.xml"))
    pom.setOwningTarget(target)
    pom.setProject(project)
    pom.setId("pom")
    target.addTask(pom)

    val task = new MavenDepsTask()
    task.setOwningTarget(target)
    task.setProject(project)
    task.addPom(pom)
    buildScopes match {
      case Some(s:String) => {
	System.out.println("Using build scopes '" + s + "'.")
	task.setScopes(s)
      }
      case _ => {
	System.out.println("Using default build scope of 'compile'.")
	task.setScopes("compile")
      }
    }
    target.addTask(task)

    project.addTarget("ResolveDependencies", target)
    project.executeTarget("ResolveDependencies")

    val f = new File(baseDir, "target/classes")
    val buildTarget = if(f.exists){Some(f)}else{None}
    System.out.println("Using build target: " + buildTarget)

    (srcDirs, task.deps, buildTarget)
  }


  def getIvyConfig(baseDir:File, buildConf:Option[String]):(Iterable[File], Iterable[File], Option[File]) = {

    val srcDirs = List("src/main/scala", "src/main/java").map(new File(_))

    System.out.println("Resolving Ivy dependencies...")
    val project = new Project()
    project.addBuildListener(newConsoleLogger)
    project.setBaseDir(baseDir)
    project.init()
    val target = new Target()
    target.setName("ResolveDependencies")
    target.setProject(project)

    val task = new IvyDepsTask()
    task.setOwningTarget(target)
    task.setProject(project)
    buildConf match {
      case Some(s:String) => {
	System.out.println("Using build config '" + s + "'.")
	task.setConf(s)
      }
      case _ =>
    }
    target.addTask(task)

    project.addTarget("ResolveDependencies", target)
    project.executeTarget("ResolveDependencies")

    val f = new File(baseDir, "target/classes")
    val buildTarget = if(f.exists){Some(f)}else{None}
    System.out.println("Using build target: " + buildTarget)

    (srcDirs, task.deps, buildTarget)
  }


  def getSbtConfig(baseDir:File, buildConf:Option[String]):(Iterable[File], Iterable[File], Option[File]) = {

    val srcDirs = List("src/main/scala", "src/main/java").map(new File(_))
    val propFile = new File(baseDir, "project/build.properties")

    if(propFile.exists){
      System.out.println("Loading sbt build properties from " + propFile)
      val conf = buildConf.getOrElse("default")
      System.out.println("Using build config '" + conf + "'")
      val sbtConfig = SbtConfigParser(propFile)
      val v = sbtConfig.buildScalaVersion
      val unmanagedLibDir = "lib"
      val managedLibDir = "lib_managed/scala_" + v + "/" + conf
      val scalaLibDir = "project/boot/scala-" + v + "/lib"
      System.out.println("Using base directory " + baseDir)
      System.out.println("Searching for dependencies in " + unmanagedLibDir)
      System.out.println("Searching for dependencies in " + managedLibDir)
      System.out.println("Searching for dependencies in " + scalaLibDir)
      var jarRoots = List(unmanagedLibDir, managedLibDir, scalaLibDir).map(new File(_))
      val jars = expandRecursively(baseDir,jarRoots,isValidJar _)
      val f = new File(baseDir, "target/scala_" + v + "/classes")
      val target = if(f.exists){Some(f)}else{None}
      System.out.println("Using build target: " + target)
      (srcDirs, jars, target)
    }
    else (srcDirs, List(), None)
  }



}



class IvyDepsTask extends IvyCacheTask() {
  var deps:Iterable[File] = List()
  def doExecute() {
    prepareAndCheck()
    deps = getArtifactReports().map{ a =>
      val art = a.asInstanceOf[ArtifactDownloadReport]
      art.getLocalFile()
    }
  }
}





