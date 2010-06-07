/* sbt -- Simple Build Tool
* Copyright 2008, 2009, 2010  Mark Harrah
*/
package com.ensime.config

import java.io.File
import java.net.URL

import org.apache.ivy.{core, plugins, util}
import core.cache.{ArtifactOrigin, CacheDownloadOptions, DefaultRepositoryCacheManager}
import core.module.descriptor.{Artifact => IvyArtifact, DefaultArtifact}
import plugins.repository.file.{FileRepository=>IvyFileRepository, FileResource}
import plugins.repository.{ArtifactResourceResolver, Resource, ResourceDownloader}
import plugins.resolver.util.ResolvedResource
import util.FileUtil


class NotInCache(val id: ModuleID, cause: Throwable) extends RuntimeException(NotInCache(id, cause), cause){
  def this(id: ModuleID) = this(id, null)
}

private object NotInCache{
  def apply(id: ModuleID, cause: Throwable) =
  {
    val postfix = if(cause == null) "" else (": " +cause.toString)
    "File for " + id + " not in cache" + postfix
  }
}


/** Provides methods for working at the level of a single jar file with the default Ivy cache.*/
object IvyCache
{
  def lockFile = new File(System.getProperty("user.home"), ".sbt.cache.lock")

  /** Get the location of the cached jar for the given ID in the Ivy cache.  If the jar is not in the cache, NotInCache is thrown .*/
  def getCachedJar(ivy:IvySbt, id: ModuleID, lock: Option[GlobalLock], log: IvyLogger):Option[File] = {
    ivy.withIvy { ivy =>
      ivy.getSettings.getRepositoryCacheManagers.flatMap{ c =>
	val cache = c.asInstanceOf[DefaultRepositoryCacheManager]
	cache.setUseOrigin(false)
	val artifact = defaultArtifact(id)
	val file = cache.getArchiveFileInCache(artifact)
	if(file.exists) Some(file)
	else None
      }.headOption
    }
  }

  private def unknownOrigin(artifact: IvyArtifact) = ArtifactOrigin.unkwnown(artifact)

  /** A minimal Ivy setup with only a local resolver and the current directory as the base directory.*/
  private def basicLocalIvy(lock: Option[GlobalLock], log: IvyLogger) = {
    val local = Resolver.defaultLocal(None)
    val paths = new IvyPaths(new File("."), None)
    val conf = new InlineIvyConfiguration(paths, Seq(local), Nil, Nil, false, lock, log)
    (new IvySbt(conf), local)
  }

  /** Creates a default jar artifact based on the given ID.*/
  private def defaultArtifact(moduleID: ModuleID): IvyArtifact =
  new DefaultArtifact(IvySbt.toID(moduleID), null, moduleID.name, "jar", "jar")
}

