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


object ExternalConfigInterface {

  private def newConsoleLogger = {
    val consoleLogger:DefaultLogger = new DefaultLogger();
    consoleLogger.setErrorPrintStream(System.err);
    consoleLogger.setOutputPrintStream(System.out);
    consoleLogger.setMessageOutputLevel(Project.MSG_INFO);
    consoleLogger
  }

  def getMavenDependencies(baseDir:File):Iterable[File] = {
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
    target.addTask(task)

    project.addTarget("ResolveDependencies", target)
    project.executeTarget("ResolveDependencies")
    task.deps
  }

  def getIvyDependencies(baseDir:File):Iterable[File] = {
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
    target.addTask(task)
    project.addTarget("ResolveDependencies", target)
    project.executeTarget("ResolveDependencies")
    task.deps
  }


  def getSbtDependencies(baseDir:File):Iterable[File] = {
    def isValidJar(f:File):Boolean = (
      f.exists && f.getPath.endsWith(".jar") && !(f.isHidden)
    )
    val propFile = new File(baseDir, "project/build.properties")
    if(propFile.exists){
      System.out.println("Loading sbt build properties from " + propFile)
      val sbtConfig = SbtConfigParser(propFile)
      val v = sbtConfig.buildScalaVersion
      val unmanagedLibDir = "lib"
      val managedLibDir = "lib_managed/scala_" + v
      val scalaLibDir = "project/boot/scala-" + v
      System.out.println("Using base directory " + baseDir)
      System.out.println("Searching for dependencies in " + unmanagedLibDir)
      System.out.println("Searching for dependencies in " + managedLibDir)
      System.out.println("Searching for dependencies in " + scalaLibDir)
      expandRecursively(baseDir,
	List(unmanagedLibDir, managedLibDir, scalaLibDir),
	isValidJar).map{s => new File(s)}
    }
    else List()
  }



}



class IvyDepsTask extends IvyCacheTask() {
  var deps:Iterable[File] = List()
  def doExecute() {
    prepareAndCheck();
    deps = getArtifactReports().map{ a =>
      val art = a.asInstanceOf[ArtifactDownloadReport]
      art.getLocalFile()
    }
  }
}





