package org.ensime.config
import java.io.File
import org.apache.ivy.{core, util}
import org.apache.ivy.ant.IvyCacheTask
import org.apache.ivy.core.report.ArtifactDownloadReport
import org.apache.maven.artifact.ant._
import org.apache.tools.ant._
import org.ensime.util.FileUtils._
import scala.collection.JavaConversions._


case class ExternalConfig(
  val sourceRoots:Iterable[File],
  val runtimeDepJars:Iterable[File],
  val compileDepJars:Iterable[File],
  val testDepJars:Iterable[File],
  val target:Option[File]){
}

object ExternalConfigInterface {

  def getMavenConfig(baseDir:File):ExternalConfig = {
    val srcDirs = makeDirs(List("src/main/scala", "src/main/java"), baseDir)
    val runtimeDeps = resolveMavenDeps(baseDir, "runtime")
    val compileDeps = resolveMavenDeps(baseDir, "compile")
    val testDeps = resolveMavenDeps(baseDir, "test")
    val f = new File(baseDir, "target/classes")
    val buildTarget = if(f.exists){Some(f)}else{None}
    ExternalConfig(srcDirs, runtimeDeps, compileDeps, testDeps, buildTarget)
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



  def getIvyConfig(baseDir:File, 
    runtimeConf:Option[String], 
    compileConf:Option[String], 
    testConf:Option[String]):ExternalConfig = {
    val srcDirs = makeDirs(List("src/main/scala", "src/main/java"), baseDir)

    val resolve = { c:String => resolveIvyDeps(baseDir, c)}

    val defaultDeps = resolve("default")
    val runtimeDeps = runtimeConf.map(resolve(_)).getOrElse(defaultDeps)
    val compileDeps = compileConf.map(resolve(_)).getOrElse(defaultDeps)
    val testDeps = testConf.map(resolve(_)).getOrElse(defaultDeps)

    val f = new File(baseDir, "target/classes")
    val buildTarget = if(f.exists){Some(f)}else{None}
    System.out.println("Using build target: " + buildTarget)

    ExternalConfig(srcDirs, runtimeDeps, compileDeps, testDeps, buildTarget)
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


  def getSbtConfig(baseDir:File):ExternalConfig = {
    val srcDirs = makeDirs(List("src"), baseDir)
    val projectProps = new File(baseDir, "project/build.properties")
    val parentProjectProps = new File(baseDir, "../project/build.properties")

    val isMain = projectProps.exists
    val isSubProject = !(projectProps.exists) && parentProjectProps.exists

    if(isMain || isSubProject){
      val propFile = if(isSubProject){ parentProjectProps } else { projectProps }
      System.out.println("Loading sbt build.properties from " + propFile + ".")
      val sbtConfig = SbtConfigParser(propFile)
      val v = sbtConfig.buildScalaVersion

      val runtimeDeps = resolveSbtDeps(baseDir, v, "runtime", isSubProject)
      val compileDeps = resolveSbtDeps(baseDir, v, "compile", isSubProject)
      val testDeps = resolveSbtDeps(baseDir, v, "test", isSubProject)

      val f = new File(baseDir, "target/scala_" + v + "/classes")
      val target = if(f.exists){Some(f)}else{None}
      System.out.println("Using build target: " + target)
      ExternalConfig(srcDirs, runtimeDeps, compileDeps, testDeps, target)
    }
    else {
      System.err.println("Could not locate build.properties file!")
      ExternalConfig(srcDirs, List(), List(), List(), None)
    }
  }

  def resolveSbtDeps(baseDir:File, scalaVersion:String, conf:String, isSubProject:Boolean):Iterable[File] = {
    System.out.println("Using build config '" + conf + "'")
    val v = scalaVersion
    val unmanagedLibDir = "lib"
    val managedLibDir = "lib_managed/scala_" + v + "/" + conf
    val defaultManagedLibDir = "lib_managed/scala_" + v + "/default"
    val scalaLibDir = if(isSubProject){"../project/boot/scala-" + v + "/lib"}
    else {"project/boot/scala-" + v + "/lib"}
    System.out.println("Using base directory " + baseDir)
    System.out.println("Searching for dependencies in " + unmanagedLibDir)
    System.out.println("Searching for dependencies in " + managedLibDir)
    System.out.println("Searching for dependencies in " + scalaLibDir)
    var jarRoots = makeDirs(List(unmanagedLibDir, managedLibDir, defaultManagedLibDir, scalaLibDir), baseDir)
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





