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


case class ExternalConfig(
  val sourceRoots:Iterable[File],
  val runtimeDepJars:Iterable[File],
  val compileDepJars:Iterable[File],
  val target:Option[File]){
}

object ExternalConfigInterface {

  def getMavenConfig(baseDir:File, runtimeScopes:Option[String], compileScopes:Option[String]):ExternalConfig = {
    val srcDirs = makeDirs(List("src/main/scala", "src/main/java"), baseDir)
    val runtimeDeps = resolveMavenDeps(baseDir, runtimeScopes.getOrElse("runtime"))
    val compileDeps = resolveMavenDeps(baseDir, compileScopes.getOrElse("compile"))
    val f = new File(baseDir, "target/classes")
    val buildTarget = if(f.exists){Some(f)}else{None}
    ExternalConfig(srcDirs, runtimeDeps, compileDeps, buildTarget)
  }


  def resolveMavenDeps(baseDir:File, scopes:String):Iterable[File] = {
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
    System.out.println("Using scopes: " + scopes)
    task.setScopes(scopes)
    target.addTask(task)

    project.addTarget("ResolveDependencies", target)
    project.executeTarget("ResolveDependencies")

    task.deps
  }



  def getIvyConfig(baseDir:File, runtimeConf:Option[String], compileConf:Option[String]):ExternalConfig = {
    val srcDirs = makeDirs(List("src/main/scala", "src/main/java"), baseDir)
    val runtimeDeps = resolveIvyDeps(baseDir, runtimeConf.getOrElse("default"))
    val compileDeps = resolveIvyDeps(baseDir, compileConf.getOrElse("default"))

    val f = new File(baseDir, "target/classes")
    val buildTarget = if(f.exists){Some(f)}else{None}
    System.out.println("Using build target: " + buildTarget)

    ExternalConfig(srcDirs, runtimeDeps, compileDeps, buildTarget)
  }

  def resolveIvyDeps(baseDir:File, conf:String):Iterable[File] = {
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
    System.out.println("Using config '" + conf + "'.")
    target.addTask(task)

    project.addTarget("ResolveDependencies", target)
    project.executeTarget("ResolveDependencies")
    task.deps
  }


  def getSbtConfig(baseDir:File, runtimeConf:Option[String], compileConf:Option[String]):ExternalConfig = {
    val srcDirs = makeDirs(List("src/main/scala", "src/main/java"), baseDir)
    val projectProps = new File(baseDir, "project/build.properties")
    val parentProjectProps = new File(baseDir, "../project/build.properties")

    val isMain = projectProps.exists
    val isSubProject = !(projectProps.exists) && parentProjectProps.exists

    if(isMain || isSubProject){
      val propFile = if(isSubProject){ parentProjectProps } else { projectProps }
      System.out.println("Loading sbt build.properties from " + propFile + ".")
      val sbtConfig = SbtConfigParser(propFile)
      val v = sbtConfig.buildScalaVersion

      val runtimeDeps = resolveSbtDeps(baseDir, v, runtimeConf.getOrElse("default"), isSubProject)
      val compileDeps = resolveSbtDeps(baseDir, v, compileConf.getOrElse("default"), isSubProject)

      val f = new File(baseDir, "target/scala_" + v + "/classes")
      val target = if(f.exists){Some(f)}else{None}
      System.out.println("Using build target: " + target)
      ExternalConfig(srcDirs, runtimeDeps, compileDeps, target)
    }
    else {
      System.err.println("Could not locate build.properties file!")
      ExternalConfig(srcDirs, List(), List(), None)
    }
  }

  def resolveSbtDeps(baseDir:File, scalaVersion:String, conf:String, isSubProject:Boolean):Iterable[File] = {
    System.out.println("Using build config '" + conf + "'")
    val v = scalaVersion
    val unmanagedLibDir = "lib"
    val managedLibDir = "lib_managed/scala_" + v + "/" + conf
    val scalaLibDir = if(isSubProject){"../project/boot/scala-" + v + "/lib"}
    else {"project/boot/scala-" + v + "/lib"}
    System.out.println("Using base directory " + baseDir)
    System.out.println("Searching for dependencies in " + unmanagedLibDir)
    System.out.println("Searching for dependencies in " + managedLibDir)
    System.out.println("Searching for dependencies in " + scalaLibDir)
    var jarRoots = makeDirs(List(unmanagedLibDir, managedLibDir, scalaLibDir), baseDir)
    val jars = expandRecursively(baseDir,jarRoots,isValidJar _)
    jars
  }

  private def newConsoleLogger = {
    val consoleLogger:DefaultLogger = new DefaultLogger()
    consoleLogger.setErrorPrintStream(System.err)
    consoleLogger.setOutputPrintStream(System.out)
    consoleLogger.setMessageOutputLevel(Project.MSG_INFO)
    consoleLogger
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





