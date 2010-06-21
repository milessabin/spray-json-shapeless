package com.ensime.util

import scala.collection.Seq
import java.io.File

// This routine stolen from http://rosettacode.org/wiki/Walk_a_directory/Recursively#Scala


/** A wrapper around file, allowing iteration either on direct children 
or on directory tree */
class RichFile(file: File) {
  
  def children = new Iterable[File] {
    override def iterator = (
      if (file.isDirectory) file.listFiles.iterator else Iterator.empty)
  }
  
  def andTree : Iterable[File] = (
    Seq(file) ++ children.flatMap(child => new RichFile(child).andTree))

}

/** implicitely enrich java.io.File with methods of RichFile */
object RichFile {
  implicit def toRichFile(file: File) = new RichFile(file)
}

object FileUtils {
  
  implicit def toRichFile(file: File) = new RichFile(file)

  def expandRecursively(rootDir:File, srcList:Iterable[String], isValid:(File => Boolean)):Set[String] = {
    (for(s <- srcList;
	val f = new File(s);
	val files = if(f.isAbsolute) f.andTree else (new File(rootDir, s)).andTree;
	file <- files if isValid(file)
      )
      yield{
	file.getAbsolutePath
      }).toSet
  }

  def expand(rootDir:File, srcList:Iterable[String], isValid:(File => Boolean)):Set[String] = {
    (for(s <- srcList;
	val f = new File(s);
	val files = List(if(f.isAbsolute) f else (new File(rootDir, s)));
	file <- files if isValid(file)
      )
      yield{
	file.getAbsolutePath
      }).toSet
  }

  def isValidJar(f:File):Boolean = f.exists && f.getName.endsWith(".jar")
  def isValidClassDir(f:File):Boolean = f.exists && f.isDirectory
  def isValidSourceFile(f:File):Boolean = {
    f.exists && !f.isHidden && (f.getName.endsWith(".scala") || f.getName.endsWith(".java"))
  }



}



