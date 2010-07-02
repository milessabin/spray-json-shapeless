package com.ensime.debug
import com.ensime.util._
import com.ensime.util.RichFile._
import com.ensime.util.FileUtils._
import com.ensime.config.ProjectConfig
import scala.collection.mutable.{HashMap, ArrayBuffer}
import java.io.File
import org.apache.bcel.classfile._

class ProjectDebugInfo(projectConfig:ProjectConfig){

  private val target:File = projectConfig.target.getOrElse(new File("."))

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
  def findUnit(source:String, line:Int, packPrefix:String):Option[DebugUnit] = {
    val units = sourceNameToUnits(source)
    units.find{ u => 
      u.startLine <= line && 
      u.endLine >= line && 
      u.packageName.startsWith(packPrefix)
    }
  }

  private val sourceNameToUnits = new HashMap[String, ArrayBuffer[DebugUnit]]{
    override def default(s:String) = new ArrayBuffer[DebugUnit]
  }

  if(target.exists && target.isDirectory){

    val classFiles = target.andTree.toList.filter{ 
      f => !f.isHidden && f.getName.endsWith(".class")
    }

    classFiles.foreach{ f =>
      val parser = new ClassParser(f.getAbsolutePath)
      val javaClass = parser.parse
      val qualName = javaClass.getClassName
      val packageName = javaClass.getPackageName
      val sourceName = javaClass.getSourceFileName

      var startLine = -1
      var endLine = -1
      javaClass.getAttributes.foreach{ 
	case lt:LineNumberTable => 
	{
	  val len = lt.getTableLength
	  val tbl = lt.getLineNumberTable
	  println("table: " + tbl.toList)
	  startLine = tbl(0).getLineNumber
	  endLine = tbl(len - 1).getLineNumber
	}
	case att => {
	  println("Found attribute: " + att)
	}
      }
      
      // Notice that a single name may resolve to many units.
      // This is either due to many classes/objects declared in one file,
      // or the fact that the mapping from source name to source path is 
      // one to many.
      val units = sourceNameToUnits(sourceName)
      val newU = new DebugUnit(startLine,endLine,f,sourceName,packageName,qualName)
      units += newU
      println("Adding.. " + newU.toString)

      // Sort in descending order of startLine, so first unit found will also be 
      // the most deeply nested.
      if(units.length > 1){
	units.sortWith{ (a,b) => a.startLine > b.startLine }
      }
    }
  }

}

class DebugUnit(
  val startLine:Int,
  val endLine:Int,
  val classFile:File,
  val sourceName:String,
  val packageName:String,
  val classQualName:String){

  override def toString = Map(
    "startLine" -> startLine,
    "endLine" -> endLine,
    "classFile" -> classFile,
    "sourceName" -> sourceName,
    "packageName" -> packageName,
    "classQualName" -> classQualName
  ).toString
}




