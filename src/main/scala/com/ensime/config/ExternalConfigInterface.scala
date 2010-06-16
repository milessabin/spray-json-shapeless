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
    val consoleLogger:DefaultLogger = new DefaultLogger()
    consoleLogger.setErrorPrintStream(System.err)
    consoleLogger.setOutputPrintStream(System.out)
    consoleLogger.setMessageOutputLevel(Project.MSG_INFO)
    consoleLogger
  }

  def getMavenDependencies(baseDir:File, config:Map[String, Any]):Iterable[File] = {
    config.get(":enabled") match{
      case Some(true) => {
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
	config.get(":scopes") match{
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
	task.deps
      }
      case None => List()
    }
  }

  def getIvyDependencies(baseDir:File, config:Map[String, Any]):Iterable[File] = {
    config.get(":enabled") match{
      case Some(true) => {
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
	config.get(":conf") match{
	  case Some(s:String) => {
	    System.out.println("Using build config '" + s + "'.")
	    task.setConf(s)
	  }
	  case _ =>
	}
	target.addTask(task)

	project.addTarget("ResolveDependencies", target)
	project.executeTarget("ResolveDependencies")
	task.deps
      }
      case None => List()
    }
  }


  def getSbtDependencies(baseDir:File, config:Map[String, Any]):Iterable[File] = {
    config.get(":enabled") match{
      case Some(true) => {
	System.out.println("Resolving sbt dependencies...")
	def isValidJar(f:File):Boolean = (
	  f.exists && f.getPath.endsWith(".jar") && !(f.isHidden)
	)
	val propFile = new File(baseDir, "project/build.properties")
	if(propFile.exists){
	  System.out.println("Loading sbt build properties from " + propFile)
	  val conf = config.get(":conf").getOrElse("default")
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
	  expandRecursively(baseDir,
	    List(unmanagedLibDir, managedLibDir, scalaLibDir),
	    isValidJar).map{s => new File(s)}
	}
	else List()
      }
      case None => List()
    }
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





