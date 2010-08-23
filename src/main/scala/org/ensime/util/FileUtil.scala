package org.ensime.util

import java.io._
import java.nio.channels.FileChannel
import java.nio.charset.Charset
import scala.collection.Seq


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

class CanonFile private (path:String) extends File(path){}

object CanonFile {
  def apply(file:File) = {
    try{
      new CanonFile(file.getCanonicalPath)
    }
    catch{
      case e:Exception => new CanonFile(file.getAbsolutePath)
    }
  }
}

object FileUtils {

  implicit def toRichFile(file: File) = new RichFile(file)

  implicit def toCanonFile(file: File):CanonFile = CanonFile(file)

  def expandRecursively(rootDir:File, fileList:Iterable[File], isValid:(File => Boolean)):Set[CanonFile] = {
    (for(f <- fileList;
	val files = if(f.isAbsolute) f.andTree else (new File(rootDir, f.getPath)).andTree;
	file <- files if isValid(file)
      )
      yield{ toCanonFile(file) }).toSet
  }

  def expand(rootDir:File, fileList:Iterable[File], isValid:(File => Boolean)):Set[CanonFile] = {
    (for(f <- fileList;
	val files = List(if(f.isAbsolute) f else (new File(rootDir, f.getPath)));
	file <- files if isValid(file)
      )
      yield{
	toCanonFile(file)
      }).toSet
  }

  def maybeDirs(names:Iterable[String], baseDir:File):Iterable[CanonFile] = {
    names.map{s => maybeDir(s,baseDir)}.flatten
  }

  def maybeFiles(names:Iterable[String], baseDir:File):Iterable[CanonFile] = {
    names.map{s => maybeFile(s,baseDir)}.flatten
  }

  def maybeFile(s:String, baseDir:File):Option[CanonFile] = { 
    val f = new File(s)
    if(f.isAbsolute) Some(toCanonFile(f))
    else Some(toCanonFile(new File(baseDir, s)))
  }.filter( f => f.exists)

  def maybeDir(s:String, baseDir:File):Option[CanonFile] = { 
    maybeFile(s, baseDir).filter(_.isDirectory)
  }

  def isValidJar(f:File):Boolean = f.exists && f.getName.endsWith(".jar")
  def isValidClassDir(f:File):Boolean = f.exists && f.isDirectory
  def isValidSourceFile(f:File):Boolean = {
    f.exists && !f.isHidden && (f.getName.endsWith(".scala") || f.getName.endsWith(".java"))
  }

  def readFile(file:File):Either[IOException, String] = {
    val cs = Charset.defaultCharset()
    try{
      val stream = new FileInputStream(file)
      try {
	val reader = new BufferedReader(new InputStreamReader(stream, cs))
	val builder = new StringBuilder()
	val buffer = new Array[Char](8192)
	var read = reader.read(buffer, 0, buffer.length)
	while(read > 0) {
          builder.appendAll(buffer, 0, read)
	  read = reader.read(buffer, 0, buffer.length)
	}
	Right(builder.toString())
      } 
      catch{
	case e:IOException => Left(e)
      }
      finally {
	stream.close()
      }
    }
    catch{
      case e:FileNotFoundException => Left(e)
    }
  }

  def replaceFileContents(file:File, newContents:String):Either[IOException, Unit] = {
    try{
      val writer = new FileWriter(file, false)
      try {
	writer.write(newContents)
	Right(())
      }
      catch{
	case e:IOException => Left(e)
      }
      finally{
	writer.close()
      }
    }
    catch{
      case e:IOException => Left(e)
    }
  }




}



