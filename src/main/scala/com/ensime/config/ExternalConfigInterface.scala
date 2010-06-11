package com.ensime.config
import java.io.File
import org.apache.ivy.{core, plugins, util, Ivy}
import core.cache.ArtifactOrigin
import core.IvyPatternHelper
import core.cache.DefaultRepositoryCacheManager
import com.ensime.util.FileUtils._

object ExternalConfigInterface {

  def getMavenDependencies(baseDir:File):Iterable[File] = {
    IvySbt.defaultMvn(baseDir) match{
      case Left(ivy) => ivy.getDependencies
      case Right(e) => List()
    }
  }

  def getIvyDependencies(baseDir:File):Iterable[File] = {
    IvySbt.defaultIvy(baseDir) match{
      case Left(ivy) => ivy.getDependencies
      case Right(e) => List()
    }
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

