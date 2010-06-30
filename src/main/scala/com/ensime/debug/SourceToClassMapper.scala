package com.ensime.debug
import com.ensime.util._
import com.ensime.util.RichFile._
import com.ensime.util.FileUtils._
import scala.collection.mutable
import java.io.File
import org.apache.bcel.classfile._

class SourceToClassMapper(baseDir:File){

  private val classToSource = new mutable.HashMap[String, String]
  private val sourceToClass = new mutable.HashMap[String, String]

  def fileForClass(klass:String) = classToSource(klass)
  def classForFile(file:File) = sourceToClass(file.getAbsolutePath)
  def getClassToSourceMap = classToSource.toMap
  def getSourceToClassMap = sourceToClass.toMap

  if(baseDir.exists && baseDir.isDirectory){
    val files:Iterable[File] = baseDir.andTree.toList.filter{ 
      f => !f.isHidden && f.getName.endsWith(".class") 
    }
    files.foreach{ f =>
      val javaClass = (new ClassParser(f.getAbsolutePath)).parse()
      val qualName = javaClass.getClassName
      val source = javaClass.getSourceFileName
      classToSource(qualName) = source
      sourceToClass(source) = qualName
      println("found class: " + qualName)
    }
  }

}
