package org.ensime.debug
import org.ensime.util._
import org.ensime.util.RichFile._
import org.ensime.util.FileUtils._
import org.ensime.config.ProjectConfig
import scala.collection.mutable.{ HashMap, ArrayBuffer }
import java.io.File
import org.apache.bcel.classfile._
import scala.math._

case class DebugSourceLinePairs(pairs: Iterable[(String, Int)])

class ProjectDebugInfo(projectConfig: ProjectConfig) {

  private val target: File = projectConfig.target.getOrElse(new File("."))

  /**
   * For each (classname,line) pair, return the corresponding
   * (sourcefile,line) pair.
   */
  def debugClassLocsToSourceLocs(pairs: Iterable[(String, Int)]): DebugSourceLinePairs = {
    DebugSourceLinePairs(pairs.map { ea =>
      findSourceForClass(ea._1) match {
        case Some(s) => (s, ea._2)
        case None => ("", ea._2)
      }
    })
  }

  /**
   *
   * Returns the unit whose bytecodes correspond to the given
   * source location.
   *
   * @param  source  The source filename, without path information.
   * @param  line    The source line
   * @param  packPrefix  A possibly incomplete prefix of the desired unit's package
   * @return         The desired unit
   */
  def findUnit(source: String, line: Int, packPrefix: String): Option[DebugUnit] = {
    val units = sourceNameToUnits(source)
    units.find { u =>
      u.startLine <= line &&
        u.endLine >= line &&
        u.packageName.startsWith(packPrefix)
    }
  }

  def findSourceForClass(className: String): Option[String] = {
    val paths = classNameToSourcePath(className)

    // TODO: Loss of precision here!
    paths.headOption
  }

  private val sourceNameToSourcePath = new HashMap[String, ArrayBuffer[String]] {
    override def default(s: String) = new ArrayBuffer[String]
  }
  projectConfig.sources.foreach { f =>
    val paths = sourceNameToSourcePath(f.getName)
    paths += f.getAbsolutePath
    sourceNameToSourcePath(f.getName) = paths
  }

  private val classNameToSourcePath = new HashMap[String, ArrayBuffer[String]] {
    override def default(s: String) = new ArrayBuffer[String]
  }

  private val sourceNameToUnits = new HashMap[String, ArrayBuffer[DebugUnit]] {
    override def default(s: String) = new ArrayBuffer[DebugUnit]
  }

  if (target.exists && target.isDirectory) {

    val classFiles = target.andTree.toList.filter { f =>
      !f.isHidden && f.getName.endsWith(".class")
    }

    classFiles.foreach { f =>
      val parser = new ClassParser(f.getAbsolutePath)
      val javaClass = parser.parse
      val qualName = javaClass.getClassName
      val packageName = javaClass.getPackageName
      val sourceName = javaClass.getSourceFileName

      val possibleSourcePaths = sourceNameToSourcePath(sourceName)
      possibleSourcePaths.foreach { p =>
        val paths = classNameToSourcePath(qualName)
        paths += p
        classNameToSourcePath(qualName) = paths
      }

      var startLine = Int.MaxValue
      var endLine = Int.MinValue

      def handleAttribute(att: Attribute) {
        att match {
          case code: Code =>
            {
              val lt = code.getLineNumberTable
              if (lt != null) {
                val tbl = lt.getLineNumberTable
                for (line <- tbl) {
                  startLine = min(startLine, line.getLineNumber)
                  endLine = max(endLine, line.getLineNumber)
                }
              }
            }
          case att => {}
        }
      }

      javaClass.getMethods.foreach { m =>
        m.getAttributes.foreach { at =>
          handleAttribute(at)
        }
      }

      // Notice that a single name may resolve to many units.
      // This is either due to many classes/objects declared in one file,
      // or the fact that the mapping from source name to source path is 
      // one to many.
      val units = sourceNameToUnits(sourceName)
      val newU = new DebugUnit(startLine, endLine, f, sourceName, packageName, qualName)
      units += newU

      // Sort in descending order of startLine, so first unit found will also be 
      // the most deeply nested.
      val sortedUnits = units.sortWith { (a, b) => a.startLine > b.startLine }

      sourceNameToUnits(sourceName) = sortedUnits
    }
    println("Finished parsing " + classFiles.length + " class files.")
  }
}

class DebugUnit(
  val startLine: Int,
  val endLine: Int,
  val classFile: File,
  val sourceName: String,
  val packageName: String,
  val classQualName: String) {

  override def toString = Map(
    "startLine" -> startLine,
    "endLine" -> endLine,
    "classFile" -> classFile,
    "sourceName" -> sourceName,
    "packageName" -> packageName,
    "classQualName" -> classQualName
    ).toString
}

