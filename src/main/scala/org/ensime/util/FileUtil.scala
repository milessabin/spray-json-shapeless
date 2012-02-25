/**
 *  Copyright (c) 2010, Aemon Cannon
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *      * Redistributions of source code must retain the above copyright
 *        notice, this list of conditions and the following disclaimer.
 *      * Redistributions in binary form must reproduce the above copyright
 *        notice, this list of conditions and the following disclaimer in the
 *        documentation and/or other materials provided with the distribution.
 *      * Neither the name of ENSIME nor the
 *        names of its contributors may be used to endorse or promote products
 *        derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED. IN NO EVENT SHALL Aemon Cannon BE LIABLE FOR ANY
 *  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 *  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 *  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 *  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.ensime.util

import java.io._
import java.nio.channels.FileChannel
import java.nio.charset.Charset
import scala.collection.Seq
import scala.collection.mutable
import scala.tools.refactoring.common.{ TextChange, NewFileChange, Change }

// This routine copied from http://rosettacode.org/wiki/Walk_a_directory/Recursively#Scala

trait FileEdit {
  def file: File
  def text: String
  def from: Int
  def to: Int
}
case class TextEdit(file: File, from: Int, to: Int, text: String) extends FileEdit {}
case class NewFile(file: File, text: String) extends FileEdit {
  def from: Int = 0
  def to: Int = text.length - 1
}
case class DeleteFile(file: File, text: String) extends FileEdit {
  def from: Int = 0
  def to: Int = text.length - 1
}

object FileEdit {

  def fromChange(ch:Change): FileEdit = {
    ch match{
      case ch: TextChange => TextEdit(ch.file.file, ch.from, ch.to, ch.text)
      case ch: NewFileChange => NewFile(ch.file, ch.text)
    }
  }

  def applyEdits(ch: List[TextEdit], source: String): String = {
    (source /: ch.sortBy(-_.to)) { (src, change) =>
      src.substring(0, change.from) + change.text + src.substring(change.to)
    }
  }

}

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

  def readFileToByteArray(file: File): Array[Byte] = {
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
      } finally {
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
      } finally {
        writer.close()
      }
    } catch {
      case e: Exception => Left(e)
    }
  }

  // Note: we assume changes do not overlap
  def inverseEdits(edits: Iterable[FileEdit]): List[FileEdit] = {
    val result = new mutable.ListBuffer[FileEdit]
    val editsByFile = edits.groupBy(_.file)
    val rewriteList = editsByFile.map {
      case (file, edits) => {
        readFile(file) match {
          case Right(contents) => {
            var dy = 0
            for (ch <- edits) {
              ch match {
                case ch: TextEdit => {
                  val original = contents.substring(ch.from, ch.to)
                  val from = ch.from + dy
                  val to = from + ch.text.length
                  result += TextEdit(ch.file, from, to, original)
                  dy += ch.text.length - original.length
                }
                case ch: NewFile => {
                  result += DeleteFile(ch.file, contents)
                }
                case ch: DeleteFile => {
                  result += NewFile(ch.file, contents)
                }
              }
            }
          }
          case Left(e) =>
        }
      }
    }
    result.toList
  }

  def writeChanges(changes: Iterable[FileEdit]): Either[Exception, Iterable[File]] = {
    val editsByFile = changes.collect { case ed: TextEdit => ed }.groupBy(_.file)
    val newFiles = changes.collect { case ed: NewFile => ed }
    try {
      val rewriteList = newFiles.map { ed => (ed.file, ed.text) } ++
        editsByFile.map {
          case (file, changes) => {
            readFile(file) match {
              case Right(contents) => {
                val newContents = FileEdit.applyEdits(changes.toList, contents)
                (file, newContents)
              }
              case Left(e) => throw e
            }
          }
        }

      val deleteFiles = changes.collect { case ed: DeleteFile => ed }
      for (ed <- deleteFiles) {
        ed.file.delete()
      }

      rewriteFiles(rewriteList) match {
        case Right(Right(())) => Right(changes.groupBy(_.file).keys)
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
        case (f: File, s: String) => if (f.isDirectory || !(f.canWrite)) {
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

