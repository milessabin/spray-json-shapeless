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

    val replCmd = m.get(key(":repl-cmd")) match{
      case Some(StringAtom(s)) => s
      case None => "scala"
    }

    val sources = new mutable.HashSet[File]
    val dependencyJars = new mutable.HashSet[File]
    val dependencyDirs = new mutable.HashSet[File]
    var target:Option[File] = None

    m.get(key(":use-sbt")) match{
      case Some(TruthAtom()) => {
	val conf = m.get(key(":sbt-conf")).map(_.toString)
	val (s,d,t) = getSbtConfig(rootDir, conf)
	sources ++= s
	dependencyJars ++= d
	target = t
      }
      case _ => 
    }

    m.get(key(":use-maven")) match{
      case Some(TruthAtom()) => {
	val scopes = m.get(key(":maven-scopes")).map(_.toString)
	val (s,d,t) = getMavenConfig(rootDir, scopes)
	sources ++= s
	dependencyJars ++= d
	target = t
      }
      case _ => 
    }

    m.get(key(":use-ivy")) match{
      case Some(TruthAtom()) => {
	val conf = m.get(key(":ivy-conf")).map(_.toString)
	val (s,d,t) = getIvyConfig(rootDir, conf)
	sources ++= s
	dependencyJars ++= d
	target = t
      }
      case _ => 
    }
    

    m.get(key(":dependency-jars")) match{
      case Some(SExpList(items)) => 
      {
	val jars = items.map{_.toString}
	dependencyJars ++= expandRecursively(rootDir, jars, isValidJar).map{s => new File(s)}
      }
      case _ => List()
    }

    m.get(key(":dependency-dirs")) match{
      case Some(SExpList(items)) => 
      {
	val dirs = items.map{_.toString}
	dependencyDirs ++= expand(rootDir,dirs,isValidClassDir).map{s => new File(s)}
      }
      case _ => List()
    }


    val includeSrcList = m.get(key(":sources")) match{
      case Some(SExpList(items)) => items.map{_.toString}
      case _ => List()
    }

    val excludeSrcList = m.get(key(":exclude-sources")) match{
      case Some(SExpList(items)) => items.map{_.toString}
      case _ => List()
    }

    sources ++= (
      expandRecursively(rootDir,includeSrcList,isValidSourceFile) -- 
      expandRecursively(rootDir,excludeSrcList,isValidSourceFile)
    ).map{s => new File(s)}

    new ProjectConfig(rootDir, sources, dependencyJars, dependencyDirs, target, replCmd)
  }



  def nullConfig = new ProjectConfig(new File("."), List(), List(), List(), None, "")

}


class ProjectConfig(
  val root:File,
  val sources:Iterable[File],
  val classpathJars:Iterable[File],
  val classpathDirs:Iterable[File],
  val target:Option[File],
  val replCmd:String){

  def compilerClasspathFilenames:Set[String] = {
    val allFiles = classpathJars ++ classpathDirs
    allFiles.map{ _.getAbsolutePath }.toSet
  }

  def replClasspathFilenames:Set[String] = {
    val allFiles = classpathJars ++ classpathDirs ++ target
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

  def replCmdLine = {
    replCmd.split(" ") ++ 
    List("-classpath", replClasspathFilenames.mkString(File.pathSeparator))
  }

  override def toString = {
    "root " + root + " \n" + 
    "sources " + sources + " \n" + 
    "classpathJars " + classpathJars + " \n" + 
    "classpathDirs " + classpathDirs + " \n"
    "target " + target + " \n"
  }
}
