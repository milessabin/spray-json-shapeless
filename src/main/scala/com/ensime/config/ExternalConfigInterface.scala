package com.ensime.config
import java.io.File
import org.apache.ivy.{core, plugins, util, Ivy}
import core.module.descriptor.DependencyDescriptor
import core.cache.ArtifactOrigin
import core.IvyPatternHelper
import core.cache.DefaultRepositoryCacheManager
import com.ensime.util.FileUtils._

object ExternalConfigInterface {

  private final class StdLogger extends IvyLogger{
    def output(msg: => String) = System.out.println(msg)
    def info(msg: => String) = {}
    def debug(msg: => String) = {}
    def warn(msg: => String) = System.err.println(msg)
    def error(msg: => String) = System.err.println(msg)
    def verbose(msg: => String) = {}
  }

  private def logger = new StdLogger
  private var baseDir = new File(".")
  private def projectID = ModuleID("aemon", "proj", "1.0")
  private def ivyLocalOnly: Boolean = true
  private def ivyRepositories: Seq[Resolver] = List()
  private def ivyValidate = true
  private def ivyPaths: IvyPaths = new IvyPaths(baseDir, None)
  private def inlineIvyConfiguration = new InlineIvyConfiguration(ivyPaths, ivyRepositories.toSeq, List(), List(), ivyLocalOnly, Some(Locks), logger)

  private def isCompileTimeDep(dep:DependencyDescriptor):Boolean = {
    val configs = dep.getModuleConfigurations
    configs.isEmpty || configs.contains("compile") || configs.contains("*")
  }

  private def newIvyModule(ivy:IvySbt, moduleSettings: ModuleSettings): IvySbt#Module = {
    val i = ivy
    new i.Module(moduleSettings)
  }



  /** 
  *  Provide an Ivy Cache Manager capable of finding Artifacts in a Maven
  *  repository cache.
  */
  private def mavenRepositoryCacheManager = {
    val m = new DefaultRepositoryCacheManager(){
      override def getArchivePathInCache(artifact:core.module.descriptor.Artifact):String = {
	val orgWithSlashes = artifact.getModuleRevisionId.getOrganisation.replace(".", "/")
	val pat = "/[module]/[revision]/[artifact]-[revision].[ext]"
        orgWithSlashes + IvyPatternHelper.substitute(pat, artifact)
      }
      override def getArchivePathInCache(artifact:core.module.descriptor.Artifact, origin:ArtifactOrigin) = {
	getArchivePathInCache(artifact)
      }
    }
    val baseDir = new File(System.getProperty("user.home"), ".m2/repository")
    m.setBasedir(baseDir.getAbsoluteFile)
    m.setUseOrigin(false)
    m
  }

  def getMavenDependencies(baseDir:File):Iterable[File] = {
    getDependencies(baseDir, true)
  }

  def getIvyDependencies(baseDir:File):Iterable[File] = {
    getDependencies(baseDir, false)
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

  private def getDependencies(baseDir:File, mvn:Boolean):Iterable[File] = {
    val externalSettings = ModuleSettings(ivyValidate, projectID)(baseDir, new StdLogger)
    try{
      val ivySbt = new IvySbt(inlineIvyConfiguration)
      val module = newIvyModule(ivySbt, externalSettings)
      module.withModule { (ivy, md, default) =>
	if(mvn){
	  ivy.getSettings.addRepositoryCacheManager(mavenRepositoryCacheManager)
	}
	md.getDependencies.flatMap{ dep =>
	  if(isCompileTimeDep(dep)){
	    IvyCache.getCachedJar(ivySbt, ModuleID(dep), Some(Locks), logger)
	  }
	  else{
	    None
	  }
	}
      }
    }
    catch{
      case e:Exception => 
      { 
	logger.error("Exception: " + e) 
	List()
      }
    }
  }

}

