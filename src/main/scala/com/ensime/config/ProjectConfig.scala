package com.ensime.config

import scala.tools.nsc.interactive.{Global, CompilerControl}
import scala.tools.nsc.{Settings}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.actors._ 
import scala.actors.Actor._ 
import com.ensime.util._
import com.ensime.util.RichFile._
import com.ensime.util.FileUtils._
import com.ensime.util.SExp._
import com.ensime.server.model._
import scala.collection.mutable
import java.io.File



object ProjectConfig{

  def apply(config:SExpList) = {
    import ExternalConfigInterface._

    val m = config.toKeywordMap

    val rootDir = m.get(key(":root-dir")) match{
      case Some(StringAtom(str)) => new File(str)
      case _ => new File(".")
    }

    val sourceRoots = new mutable.HashSet[File]
    val dependencyJars = new mutable.HashSet[File]
    val dependencyDirs = new mutable.HashSet[File]
    var target:Option[File] = None

    m.get(key(":use-sbt")) match{
      case Some(TruthAtom()) => {
	val conf = m.get(key(":sbt-compile-conf")).map(_.toString)
	val (s,d,t) = getSbtConfig(rootDir, conf)
	sourceRoots ++= s
	dependencyJars ++= d
	target = t
      }
      case _ => 
    }

    m.get(key(":use-maven")) match{
      case Some(TruthAtom()) => {
	val scopes = m.get(key(":maven-compile-scopes")).map(_.toString)
	val (s,d,t) = getMavenConfig(rootDir, scopes)
	sourceRoots ++= s
	dependencyJars ++= d
	target = t
      }
      case _ => 
    }

    m.get(key(":use-ivy")) match{
      case Some(TruthAtom()) => {
	val conf = m.get(key(":ivy-compile-conf")).map(_.toString)
	val (s,d,t) = getIvyConfig(rootDir, conf)
	sourceRoots ++= s
	dependencyJars ++= d
	target = t
      }
      case _ => 
    }
    

    m.get(key(":dependency-jars")) match{
      case Some(SExpList(items)) => 
      {
	val jars = items.map{j => new File(j.toString)}
	dependencyJars ++= expandRecursively(rootDir, jars, isValidJar _)
      }
      case _ => List()
    }

    m.get(key(":dependency-dirs")) match{
      case Some(SExpList(items)) => 
      {
	val dirs = items.map{d => new File(d.toString)}
	dependencyDirs ++= expand(rootDir,dirs,isValidClassDir _)
      }
      case _ => List()
    }


    val includeSrcList = m.get(key(":sources")) match{
      case Some(SExpList(items)) => items.map{_.toString}
      case _ => List()
    }

    sourceRoots ++= includeSrcList.map(new File(_)).filter(_.isDirectory)

    val sourceFiles = expandRecursively(
      rootDir,sourceRoots,isValidSourceFile _)

    new ProjectConfig(rootDir, sourceFiles, sourceRoots, 
      dependencyJars, dependencyDirs, target)
  }



  def nullConfig = new ProjectConfig(new File("."), List(), 
    List(), List(), List(), None)

}


class ProjectConfig(
  val root:File,
  val sources:Iterable[File],
  val sourceRoots:Iterable[File],
  val classpathJars:Iterable[File],
  val classpathDirs:Iterable[File],
  val target:Option[File]){

  def compilerClasspathFilenames:Set[String] = {
    val allFiles = classpathJars ++ classpathDirs
    allFiles.map{ _.getAbsolutePath }.toSet
  }

  def sourceFilenames:Set[String] = {
    sources.map{ _.getAbsolutePath }.toSet
  }

  def compilerArgs = List(
    "-classpath", compilerClasspathFilenames.mkString(File.pathSeparator),
    "-verbose",
    sourceFilenames.mkString(" ")
  )

  def replClasspath = {
    val allFiles = classpathJars ++ classpathDirs ++ target
    val paths = allFiles.map{ _.getAbsolutePath }.toSet
    paths.mkString(File.pathSeparator)
  }

  def debugClasspath = replClasspath

  def debugSourcepath = {
    sourceRoots.map{ _.getAbsolutePath }.toSet.mkString(File.pathSeparator)
  }

  override def toString = {
    "root " + root + " \n" + 
    "sources " + sources + " \n" + 
    "classpathJars " + classpathJars + " \n" + 
    "classpathDirs " + classpathDirs + " \n"
    "target " + target + " \n"
  }
}
