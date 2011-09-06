/**
*  Copyright (C) 2010 Aemon Cannon
*
*  This program is free software; you can redistribute it and/or
*  modify it under the terms of the GNU General Public License as
*  published by the Free Software Foundation; either version 2 of
*  the License, or (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public
*  License along with this program; if not, write to the Free
*  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
*  MA 02111-1307, USA.
*/

package org.ensime.util

import java.io._
import java.nio.channels.FileChannel
import java.nio.charset.Charset
import scala.collection.Seq
import scala.collection.mutable
import scala.tools.refactoring.common.Change

// This routine copied from http://rosettacode.org/wiki/Walk_a_directory/Recursively#Scala

/** A wrapper around file, allowing iteration either on direct children or on directory tree */
class RichFile(file: File) {

  def children = new Iterable[File] {
    override def iterator = (if (file.isDirectory) file.listFiles.iterator else Iterator.empty)
  }

  def andTree: Iterable[File] = (Seq(file) ++ children.flatMap(child => new RichFile(child).andTree))

}

/** implicitely enrich java.io.File with methods of RichFile */
object RichFile {
  implicit def toRichFile(file: File) = new RichFile(file)
}

class CanonFile private (path: String) extends File(path) {}

object CanonFile {
  def apply(file: File) = {
    try {
      new CanonFile(file.getCanonicalPath)
    } catch {
      case e: Exception => new CanonFile(file.getAbsolutePath)
    }
  }
  def apply(str: String) = {
    try {
      new CanonFile(new File(str).getCanonicalPath)
    } catch {
      case e: Exception => new CanonFile(str)
    }
  }
}

object FileUtils {

  implicit def toRichFile(file: File) = new RichFile(file)

  implicit def toCanonFile(file: File): CanonFile = CanonFile(file)

  def expandRecursively(rootDir: File, fileList: Iterable[File], isValid: (File => Boolean)): Set[CanonFile] = {
    (for (
	f <- fileList;
	val files = if (f.isAbsolute) f.andTree else (new File(rootDir, f.getPath)).andTree;
	file <- files if isValid(file)
      ) yield { toCanonFile(file) }).toSet
  }

  def expand(rootDir: File, fileList: Iterable[File], isValid: (File => Boolean)): Set[CanonFile] = {
    (for (
	f <- fileList;
	val files = List(if (f.isAbsolute) f else (new File(rootDir, f.getPath)));
	file <- files if isValid(file)
      ) yield {
	toCanonFile(file)
      }).toSet
  }

  def maybeDirs(names: Iterable[String], baseDir: File): Iterable[CanonFile] = {
    names.map { s => maybeDir(s, baseDir) }.flatten
  }

  def maybeFiles(names: Iterable[String], baseDir: File): Iterable[CanonFile] = {
    names.map { s => maybeFile(s, baseDir) }.flatten
  }

  def maybeFile(s: String, baseDir: File): Option[CanonFile] = {
    val f = new File(s)
    if (f.isAbsolute) Some(toCanonFile(f))
    else Some(toCanonFile(new File(baseDir, s)))
  }.filter(f => f.exists)

  def maybeDir(s: String, baseDir: File): Option[CanonFile] = {
    maybeFile(s, baseDir).filter(_.isDirectory)
  }

  def isValidJar(f: File): Boolean = f.exists && f.getName.endsWith(".jar")
  def isValidClassDir(f: File): Boolean = f.exists && f.isDirectory
  def isValidSourceFile(f: File): Boolean = {
    f.exists && !f.isHidden && (f.getName.endsWith(".scala") || f.getName.endsWith(".java"))
  }

  def readFileToByteArray(file:File):Array[Byte] = {  
    val length = file.length.toInt
    val array = new Array[Byte](length)
    val in = new FileInputStream(file)
    var offset = 0
    while (offset < length) {
      var count = in.read(array, offset, (length - offset))
      offset += count
    }  
    in.close() 
    return array
  }  

  def readFile(file: File): Either[IOException, String] = {
    val cs = Charset.defaultCharset()
    try {
      val stream = new FileInputStream(file)
      try {
        val reader = new BufferedReader(new InputStreamReader(stream, cs))
        val builder = new StringBuilder()
        val buffer = new Array[Char](8192)
        var read = reader.read(buffer, 0, buffer.length)
        while (read > 0) {
          builder.appendAll(buffer, 0, read)
          read = reader.read(buffer, 0, buffer.length)
        }
        Right(builder.toString())
      } catch {
        case e: IOException => Left(e)
      }
      finally {
        stream.close()
      }
    } catch {
      case e: FileNotFoundException => Left(e)
    }
  }

  def replaceFileContents(file: File, newContents: String): Either[Exception, Unit] = {
    try {
      val writer = new FileWriter(file, false)
      try {
        writer.write(newContents)
        Right(())
      } catch {
        case e: IOException => Left(e)
      }
      finally {
        writer.close()
      }
    } catch {
      case e: Exception => Left(e)
    }
  }

  // Note: we assume changes do not overlap
  def inverseChanges(changes: Iterable[Change]): List[Change] = {
    val result = new mutable.ListBuffer[Change]
    val changesByFile = changes.groupBy(_.file.file)
    val rewriteList = changesByFile.map {
      case (file, changes) => {
        readFile(file) match {
          case Right(contents) => {
            var dy = 0
            for (ch <- changes) {
              val original = contents.substring(ch.from, ch.to)
              val from = ch.from + dy
              val to = from + ch.text.length
              result += Change(ch.file, from, to, original)
              dy += ch.text.length - original.length
            }
          }
          case Left(e) =>
        }
      }
    }
    result.toList
  }

  def writeChanges(changes: Iterable[Change]): Either[Exception, Iterable[File]] = {
    val changesByFile = changes.groupBy(_.file.file)
    try {
      val rewriteList = changesByFile.map {
        case (file, changes) => {
          readFile(file) match {
            case Right(contents) => {
              val changed = Change.applyChanges(changes.toList, contents)
              (file, changed)
            }
            case Left(e) => throw e
          }
        }
      }
      rewriteFiles(rewriteList) match {
        case Right(Right(())) => Right(changesByFile.keys)
        case Right(Left(e)) => Left(new IllegalStateException(
            "Possibly incomplete write of change-set caused by: " + e))
        case Left(e) => Left(e)
      }
    } catch {
      case e: Exception => Left(e)
    }
  }

  /**
  * For each (f,s) pair, replace the contents of f with s. If any errors occurs
  * before any disk writes, return Left(exception). If  an error occurs DURING
  * disk writes, return Right(Left(exception)). Otherwise, return Right(Right(()))
  */
  def rewriteFiles(changes: Iterable[(File, String)]): Either[Exception, Either[Exception, Unit]] = {
    val touchedFiles = new mutable.ListBuffer[File]
    try {

      // Try to fail fast, before writing anything to disk.
      changes.foreach {
        case (f: File, s: String) => if (!(f.exists) || f.isDirectory || !(f.canWrite)) {
          throw new IllegalArgumentException(f + " is not a writable file.")
        }
        case _ => {
          throw new IllegalArgumentException("Invalid (File,String) pair.")
        }
      }

      // Apply the changes. An error here may result in a corrupt disk state :(
      changes.foreach {
        case (file, newContents) => {
          replaceFileContents(file, newContents) match {
            case Right(_) => {}
            case Left(e) => Right(Left(e))
          }
        }
      }

      Right(Right(()))

    } catch {
      case e: Exception => Left(e)
    }
  }

}

