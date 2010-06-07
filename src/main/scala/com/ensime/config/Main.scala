package com.ensime.config
import java.io.File
import org.apache.ivy.{core, plugins, util, Ivy}
import core.module.descriptor.DependencyDescriptor
import core.cache.ArtifactOrigin
import core.IvyPatternHelper
import core.cache.DefaultRepositoryCacheManager

object Main {

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
  def projectID = ModuleID("aemon", "proj", "1.0")
  def ivyLocalOnly: Boolean = true
  def ivyRepositories: Seq[Resolver] = List()
  def ivyValidate = true
  def ivyPaths: IvyPaths = new IvyPaths(baseDir, None)
  def inlineIvyConfiguration = new InlineIvyConfiguration(ivyPaths, ivyRepositories.toSeq, List(), List(), ivyLocalOnly, Some(Locks), logger)
  def externalSettings = ModuleSettings(ivyValidate, projectID)(baseDir, new StdLogger)
  def newIvyModule(ivy:IvySbt, moduleSettings: ModuleSettings): IvySbt#Module = {
    val i = ivy
    new i.Module(moduleSettings)
  }

  def isCompileTimeDep(dep:DependencyDescriptor):Boolean = {
    val configs = dep.getModuleConfigurations
    configs.isEmpty || configs.contains("compile") || configs.contains("*")
  }

  def mavenRepositoryCacheManager = {
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

  def main(args: Array[String]): Unit = {
    if(args.length == 1){
      baseDir = new File(args(0))
      try{
	val ivySbt = new IvySbt(inlineIvyConfiguration)
	val module = newIvyModule(ivySbt, externalSettings)
	module.withModule { (ivy, md, default) =>
	  ivy.getSettings.addRepositoryCacheManager(mavenRepositoryCacheManager)
	  for(dep <- md.getDependencies){
	    if(isCompileTimeDep(dep)){
	      IvyCache.getCachedJar(ivySbt, ModuleID(dep), Some(Locks), logger) match{
		case Some(f) => logger.output(f.getAbsolutePath)
		case None => 
	      }
	    }
	  }
	}
      }
      catch{
	case e:Exception => logger.error("Exception: " + e)
      }
    }
    else{
      println("Usage: cmd project-root")
    }
  }

}

