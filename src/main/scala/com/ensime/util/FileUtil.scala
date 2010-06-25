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

  def expandRecursively(rootDir:File, fileList:Iterable[File], isValid:(File => Boolean)):Set[File] = {
    (for(f <- fileList;
	val files = if(f.isAbsolute) f.andTree else (new File(rootDir, f.getPath)).andTree;
	file <- files if isValid(file)
      )
      yield{ file }).toSet
  }

  def expand(rootDir:File, fileList:Iterable[File], isValid:(File => Boolean)):Set[File] = {
    (for(f <- fileList;
	val files = List(if(f.isAbsolute) f else (new File(rootDir, f.getPath)));
	file <- files if isValid(file)
      )
      yield{
	file
      }).toSet
  }

  def makeDirs(names:Iterable[String], baseDir:File):Iterable[File] = {
    names.map{ s =>
      val f = new File(s)
      if(f.isAbsolute && f.isDirectory) f
      else new File(baseDir, s)
    }.filter( f => f.exists)
  }

  def makeFiles(names:Iterable[String], baseDir:File):Iterable[File] = {
    names.map{ s =>
      val f = new File(s)
      if(f.isAbsolute) f
      else new File(baseDir, s)
    }.filter( f => f.exists)
  }


  def isValidJar(f:File):Boolean = f.exists && f.getName.endsWith(".jar")
  def isValidClassDir(f:File):Boolean = f.exists && f.isDirectory
  def isValidSourceFile(f:File):Boolean = {
    f.exists && !f.isHidden && (f.getName.endsWith(".scala") || f.getName.endsWith(".java"))
  }



}



