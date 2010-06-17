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
import java.io.File



object ProjectConfig{

  def apply(config:SExpList) = {
    val m = config.toKeywordMap
    val root = m.get(key(":root-dir")) match{
      case Some(StringAtom(str)) => str
      case _ => "."
    }
    val src = (m.get(key(":source")) match{
	case Some(s:SExpList) => s
	case _ => SExpList(List())
      }).toKeywordMap

    val includeSrcList = src.get(key(":include")) match{
      case Some(SExpList(items)) => items.map{_.toString}
      case _ => List()
    }
    val excludeSrcList = src.get(key(":exclude")) match{
      case Some(SExpList(items)) => items.map{_.toString}
      case _ => List()
    }

    val cp = (m.get(key(":classpath")) match{
	case Some(c:SExpList) => c
	case _ => SExpList(List())
      }).toKeywordMap


    def getDependencyConfig(s:Option[SExp]):Map[String, Any] = {
      s match{
	case Some(s:SExpList) => {
	  val keyMap = s.toKeywordMap
	  val map = keyMap.map{ ea => (ea._1.toString, ea._2.toScala)}
	  map ++ Map(":enabled" -> true)
	}
	case Some(c:TruthAtom) => Map(":enabled" -> true)
	case _ => Map()
      }
    }
    
    val mvnMap = getDependencyConfig(cp.get(key(":mvn")))
    val ivyMap = getDependencyConfig(cp.get(key(":ivy")))
    val sbtMap = getDependencyConfig(cp.get(key(":sbt")))

    val jarList = cp.get(key(":jars")) match{
      case Some(SExpList(items)) => items.map{_.toString}
      case _ => List()
    }

    val dirsList = cp.get(key(":dirs")) match{
      case Some(SExpList(items)) => items.map{_.toString}
      case _ => List()
    }

    val rootDir = new File(root)
    val sourceFiles = (
      expandRecursively(rootDir,includeSrcList,isValidSourceFile) -- 
      expandRecursively(rootDir,excludeSrcList,isValidSourceFile)
    ).map{s => new File(s)}


    val jarFiles = (
      expandRecursively(rootDir, jarList, isValidJar).map{s => new File(s)} ++ 
      ExternalConfigInterface.getIvyDependencies(rootDir, ivyMap) ++ 
      ExternalConfigInterface.getMavenDependencies(rootDir, mvnMap) ++ 
      ExternalConfigInterface.getSbtDependencies(rootDir, sbtMap))

    val dirFiles = expand(rootDir,jarList, isValidClassDir).map{s => new File(s)}

    new ProjectConfig(rootDir, sourceFiles, jarFiles, dirFiles)
  }


  private def isValidJar(f:File):Boolean = f.exists
  private def isValidClassDir(f:File):Boolean = f.isDirectory && f.exists
  private def isValidSourceFile(f:File):Boolean = {
    f.exists && !f.isHidden && (f.getName.endsWith(".scala") || f.getName.endsWith(".java"))
  }

  def nullConfig = new ProjectConfig(new File("."), List(), List(), List())

}


class ProjectConfig(
  val root:File,
  val sources:Iterable[File],
  val classpathJars:Iterable[File],
  val classpathDirs:Iterable[File]){

  def classpathFilenames:Set[String] = {
    val allFiles = classpathJars ++ classpathDirs
    allFiles.map{ _.getAbsolutePath }.toSet
  }

  def sourceFilenames:Set[String] = {
    sources.map{ _.getAbsolutePath }.toSet
  }

  def compilerArgs = List(
    "-classpath", classpathFilenames.mkString(File.pathSeparator),
    "-verbose",
    sourceFilenames.mkString(" ")
  )

  def replArgs = List(
    "-classpath", classpathFilenames.mkString(File.pathSeparator)
  )

  override def toString = {
    "root " + root + " \n" + 
    "sources " + sources + " \n" + 
    "classpathJars " + classpathJars + " \n" + 
    "classpathDirs " + classpathDirs + " \n"
  }
}
