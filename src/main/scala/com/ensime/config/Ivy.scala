/* sbt -- Simple Build Tool
* Copyright 2008, 2009, 2010 Mark Harrah
*/
package com.ensime.config

import java.io.File
import java.util.concurrent.Callable

import org.apache.ivy.{core, plugins, util, Ivy}
import core.IvyPatternHelper
import core.cache.DefaultRepositoryCacheManager
import core.module.descriptor.{DefaultArtifact, DefaultDependencyArtifactDescriptor, MDArtifact}
import core.module.descriptor.{DefaultDependencyDescriptor, DefaultModuleDescriptor,  ModuleDescriptor}
import core.module.id.{ArtifactId, ModuleId, ModuleRevisionId}
import core.settings.IvySettings
import core.module.descriptor.DependencyDescriptor
import core.cache.ArtifactOrigin
import core.resolve.ResolveOptions
import plugins.matcher.PatternMatcher
import plugins.parser.m2.PomModuleDescriptorParser
import plugins.resolver.ChainResolver
import util.Message
import scala.collection.JavaConversions._



final class IvySbt(configuration: IvyConfiguration){

  import configuration.{log, baseDirectory}

  /** ========== Configuration/Setup ============
  * This part configures the Ivy instance by first creating the logger interface to ivy, then IvySettings, and then the Ivy instance.
  * These are lazy so that they are loaded within the right context.  This is important so that no Ivy XML configuration needs to be loaded,
  * saving some time.  This is necessary because Ivy has global state (IvyContext, Message, DocumentBuilder, ...).
  */

  private lazy val logger = new IvyLoggerInterface(log)

  private lazy val ivyLockFile = new File(settings.getDefaultIvyUserDir, ".ensime.ivy.lock")

  private def withDefaultLogger[T](f: => T): T = {

    def action() = IvySbt.synchronized {
      val originalLogger = Message.getDefaultLogger
      Message.setDefaultLogger(logger)
      try { f }
      finally { Message.setDefaultLogger(originalLogger) }
    }

    // Ivy is not thread-safe nor can the cache be used concurrently.
    // If provided a GlobalLock, we can use that to ensure safe access to the cache.
    // Otherwise, we can at least synchronize within the JVM.
    // For thread-safety In particular, Ivy uses a static DocumentBuilder, which is not thread-safe.
    configuration.lock match
    {
      case Some(lock) => lock(ivyLockFile, new Callable[T] { def call = action() })
      case None => action()
    }

  }

  /** 
  *  Provide an Ivy Cache Manager capable of finding Artifacts in a Maven
  *  repository cache.
  */
  private def mavenRepositoryCacheManager(settings:IvySettings, cacheDir:File) = {
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
    m.setBasedir(cacheDir.getAbsoluteFile)
    m.setUseOrigin(false)
    m
  }


  private def ivyRepositoryCacheManager(settings:IvySettings, cacheDir:File) = {
    val m = new DefaultRepositoryCacheManager("default-cache", settings, cacheDir)
    m.setUseOrigin(false)
    m
  }


  private lazy val settings = {
    val is = new IvySettings
    is.setBaseDir(baseDirectory)
    val manager = configuration match {
      case pc:MvnFileConfiguration => 
      {
	mavenRepositoryCacheManager(is, configuration.cacheBaseDirectory)
      }
      case ifc:IvyFileConfiguration => 
      {
	ivyRepositoryCacheManager(is, configuration.cacheBaseDirectory)
      }
    }
    is.addRepositoryCacheManager(manager)
    is.setDefaultRepositoryCacheManager(manager)
    is.setDefaultResolutionCacheBasedir(configuration.cacheBaseDirectory.getAbsolutePath)
    is
  }

  private lazy val ivy = {
    val i = new Ivy()
    i.setSettings(settings)
    i.bind()
    i.getLoggerEngine.pushLogger(logger)
    i
  }

  /** Uses the configured Ivy instance within a safe context.*/
  def withIvy[T](f: Ivy => T): T = withDefaultLogger {
    ivy.pushContext()
    try { f(ivy) }
    finally { ivy.popContext() }
  }

  def withModule[T](f:(Ivy,DefaultModuleDescriptor) => T): T = withIvy[T] { 
    ivy => f(ivy, moduleDescriptor)
  }

  private lazy val moduleDescriptor: DefaultModuleDescriptor = {
    val baseModule = configuration match {
      case pc: MvnFileConfiguration => readPom(pc.file, true)
      case ifc: IvyFileConfiguration => readIvyFile(ifc.file, true)
    }
    baseModule
  }

  /** Parses the given Maven pom 'pomFile'.*/
  private def readPom(pomFile: File, validate: Boolean) = {
    val md = PomModuleDescriptorParser.getInstance.parseDescriptor(settings, toURL(pomFile), validate)
    toDefaultModuleDescriptor(md)
  }

  /** Parses the given Ivy file 'ivyFile'.*/
  private def readIvyFile(ivyFile: File, validate: Boolean) = {
    val url = toURL(ivyFile)
    val parser = new CustomXmlParser.CustomParser(settings, None)
    parser.setValidate(validate)
    parser.setSource(url)
    parser.parse()
    val md = parser.getModuleDescriptor()
    toDefaultModuleDescriptor(md)
  }


  def getDependencies():Iterable[File] = {
    withModule { (ivy, md) =>
      md.getDependencies.flatMap{ dep =>

	val mid = dep.getDependencyRevisionId

	val opts = new ResolveOptions()
	opts.setConfs(Array("compile"))
	opts.setTransitive(true)
	opts.setUseCacheOnly(true)
	opts.setValidate(false)
	opts.setDownload(false)
	opts.setCheckIfChanged(false)
	
	val report = ivy.resolve(mid, opts, true)

	if(isCompileTimeDep(dep)){
	  IvyCache.getCachedJar(this, dep.getDependencyRevisionId, Some(Locks))
	}
	else{
	  None
	}

      }
    }
  }

  private def isCompileTimeDep(dep:DependencyDescriptor):Boolean = {
    val configs = dep.getModuleConfigurations
    configs.isEmpty || configs.contains("compile") || configs.contains("*")
  }

  private def toURL(file: File) = file.toURI.toURL

  /** This code converts the given ModuleDescriptor to a DefaultModuleDescriptor by casting or generating an error.
  * Ivy 2.0.0 always produces a DefaultModuleDescriptor. */
  private def toDefaultModuleDescriptor(md: ModuleDescriptor) = md match {
    case dmd: DefaultModuleDescriptor => dmd
    case _ => error("Unknown ModuleDescriptor type.")
  }

}


private object IvySbt{

  val DefaultIvyConfigFilename = "ivysettings.xml"

  val DefaultIvyFilename = "ivy.xml"

  val DefaultMavenFilename = "pom.xml"

  val DefaultMavenCacheDir = new File(System.getProperty("user.home"), ".m2/repository")

  val DefaultIvyCacheDir = new File(System.getProperty("user.home"), ".ivy2/cache")

  def defaultIvyFile(project: File) = new File(project, DefaultIvyFilename)

  def defaultIvyConfiguration(project: File) = new File(project, DefaultIvyConfigFilename)

  def defaultPOM(project: File) = new File(project, DefaultMavenFilename)


  def defaultMvn(baseDir:File):Either[IvySbt, Throwable] = {
    val f = new File(baseDir, DefaultMavenFilename)
    val cacheF = DefaultMavenCacheDir
    if(!f.exists) Right(new Exception("pom.xml not found in project base directory."))
    else if(!cacheF.exists) Right(new Exception("Mvn cache not found!"))
    else {
      try{
	Left(new IvySbt(
	    new MvnFileConfiguration(
	      baseDir,
	      Some(Locks),
	      new StdLogger,
	      f,
	      cacheF)))
      }
      catch{
	case e:Exception => Right(e)
      }
    }
  }

  def defaultIvy(baseDir:File):Either[IvySbt, Throwable] = {
    val f = new File(baseDir, DefaultIvyFilename)
    val cacheF = DefaultIvyCacheDir
    if(!f.exists) Right(new Exception("ivy.xml not found in project base directory."))
    else if(!cacheF.exists) Right(new Exception("Ivy cache not found!"))
    else {
      try{
	Left(new IvySbt(
	    new IvyFileConfiguration(
	      baseDir, 
	      Some(Locks),
	      new StdLogger,
	      f,
	      cacheF)))
      }
      catch{
	case e:Exception => Right(e)
      }
    }
  }


}
