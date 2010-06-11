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
import core.module.id.{ArtifactId, ModuleId, ModuleRevisionId}
import plugins.resolver.util.ResolvedResource
import util.FileUtil


/** Provides methods for working at the level of a single jar file with the default Ivy cache.*/
object IvyCache
{

  def lockFile = new File(System.getProperty("user.home"), ".sbt.cache.lock")

  /** Get the location of the cached jar for the given ID in the Ivy cache.  If the jar is not in the cache, NotInCache is thrown .*/
  def getCachedJar(ivy:IvySbt, id: ModuleRevisionId, lock: Option[GlobalLock]):Option[File] = {
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

  /** Creates a default jar artifact based on the given ID.*/
  private def defaultArtifact(id: ModuleRevisionId): IvyArtifact = 
  new DefaultArtifact(id, null, id.getName, "jar", "jar")
}

