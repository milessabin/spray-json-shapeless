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

  def getMavenDependencies(baseDir:File):Iterable[File] = {
    MavenDeps.getDeps(baseDir)    
  }

  def getIvyDependencies(baseDir:File):Iterable[File] = {
    IvyDeps.getDeps(baseDir)
  }


  def getSbtDependencies(baseDir:File):Iterable[File] = {
    def isValidJar(f:File):Boolean = f.exists
    val propFile = new File(baseDir, "project/build.properties")
    if(propFile.exists){
      val sbtConfig = SbtConfigParser(propFile)
      val v = sbtConfig.buildScalaVersion
      val unmanagedLibDir = "lib"
      val managedLibDir = "lib_managed/scala_" + v
      val scalaLibDir = "project/boot/scala-" + v
      expandRecursively(baseDir,
	List(unmanagedLibDir, managedLibDir,scalaLibDir),
	isValidJar).map{s => new File(s)}
    }
    else List()
  }


}


object DebugBuildListener extends BuildListener{
  def buildFinished(event:BuildEvent) = debug(event)
  def buildStarted(event:BuildEvent) = debug(event)
  def messageLogged(event:BuildEvent) = debug(event)
  def targetFinished(event:BuildEvent) = debug(event)
  def targetStarted(event:BuildEvent) = debug(event)
  def taskFinished(event:BuildEvent) = debug(event)   
  def taskStarted(event:BuildEvent) = debug(event)   
  def debug(event:BuildEvent) = {
    val exc = event.getException
    if(exc != null){
    }
  }
}


object IvyDeps {

  def getDeps(baseDir:File):Iterable[File] = {
    val project = new Project()
    val consoleLogger:DefaultLogger = new DefaultLogger();
    consoleLogger.setErrorPrintStream(System.err);
    consoleLogger.setOutputPrintStream(System.out);
    consoleLogger.setMessageOutputLevel(Project.MSG_INFO);
    project.addBuildListener(consoleLogger);
    project.setBaseDir(baseDir)
    project.init()
    val target = new Target()
    target.setName("IvyDeps")
    target.setProject(project)
    val task = new IvyDepsTask()
    task.setOwningTarget(target)
    task.setProject(project)
    task.setConf("compile")
    target.addTask(task)
    project.addTarget("IvyDeps", target)
    project.executeTarget("IvyDeps")
    task.deps
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



object MavenDeps {

  def getDeps(baseDir:File):Iterable[File] = {
    val project = new Project()
    project.addBuildListener(DebugBuildListener)
    project.setBaseDir(baseDir)
    val target = new Target()
    val task = new MavenDepsTask()
    target.addTask(task)
    target.setProject(project)
    project.addTarget("deps", target)
    project.executeTarget("deps")
    task.deps
  }

}


