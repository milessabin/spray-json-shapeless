package org.ensime.util

import java.io._
import java.net.URI
import java.nio.charset.Charset

import org.apache.commons.vfs2.FileObject

import scala.collection.mutable

import org.ensime.api._

class RichFileObject(fo: FileObject) {
  // None if the fo is not an entry in an archive
  def pathWithinArchive: Option[String] = {
    val uri = fo.getName.getURI
    if (uri.startsWith("jar") || uri.startsWith("zip"))
      Some(fo.getName.getRoot.getRelativeName(fo.getName))
    else None
  }

  // assumes it is a local file
  def asLocalFile: File = {
    require(fo.getName.getURI.startsWith("file"))
    new File(new URI(fo.getName.getURI))
  }
}

object RichFileObject {
  implicit def toRichFileObject(fo: FileObject): RichFileObject = new RichFileObject(fo)
}

object FileUtils {

  def isScalaSourceFile(f: File): Boolean = {
    f.exists && f.getName.endsWith(".scala")
  }

  def readFile(f: File, cs: Charset): Either[IOException, String] = try {
    // impl will be replaced by https://github.com/stacycurl/pimpathon/issues/201
    val ram = new RandomAccessFile(f, "r")
    val bytes = Array.ofDim[Byte](f.length.intValue())
    ram.read(bytes)
    Right(new String(bytes, cs))
  } catch {
    case e: IOException => Left(e)
  }

  def replaceFileContents(file: File, newContents: String, cs: Charset): Either[Exception, Unit] = {
    try {
      val stream = new FileOutputStream(file)
      val writer = new OutputStreamWriter(stream, cs)
      try {
        writer.write(newContents)
        Right(())
      } catch {
        case e: IOException =>
          Left(e)
      } finally {
        writer.close()
      }
    } catch {
      case e: Exception =>
        Left(e)
    }
  }

  // Note: we assume changes do not overlap
  def inverseEdits(edits: Iterable[FileEdit], cs: Charset): List[FileEdit] = {
    val result = new mutable.ListBuffer[FileEdit]
    val editsByFile = edits.groupBy(_.file)
    editsByFile.foreach {
      case (file, fileEdits) =>
        readFile(file, cs) match {
          case Right(contents) =>
            var dy = 0
            for (ch <- fileEdits) {
              ch match {
                case ch: TextEdit =>
                  val original = contents.substring(ch.from, ch.to)
                  val from = ch.from + dy
                  val to = from + ch.text.length
                  result += TextEdit(ch.file, from, to, original)
                  dy += ch.text.length - original.length
                case ch: NewFile =>
                  result += DeleteFile(ch.file, contents)
                case ch: DeleteFile =>
                  result += NewFile(ch.file, contents)
              }
            }
          case Left(e) =>
        }
    }
    result.toList
  }

  def writeChanges(changes: Iterable[FileEdit], cs: Charset): Either[Exception, Iterable[File]] = {
    val editsByFile = changes.collect { case ed: TextEdit => ed }.groupBy(_.file)
    val newFiles = changes.collect { case ed: NewFile => ed }
    try {
      val rewriteList = newFiles.map { ed => (ed.file, ed.text) } ++
        editsByFile.map {
          case (file, fileChanges) =>
            readFile(file, cs) match {
              case Right(contents) =>
                val newContents = FileEditHelper.applyEdits(fileChanges.toList, contents)
                (file, newContents)
              case Left(e) => throw e
            }
        }

      val deleteFiles = changes.collect { case ed: DeleteFile => ed }
      for (ed <- deleteFiles) {
        ed.file.delete()
      }

      rewriteFiles(rewriteList, cs) match {
        case Right(Right(_)) => Right(changes.groupBy(_.file).keys)
        case Right(Left(e)) => Left(new IllegalStateException(
          "Possibly incomplete write of change-set caused by: " + e
        ))
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
  def rewriteFiles(changes: Iterable[(File, String)], cs: Charset): Either[Exception, Either[Exception, Unit]] = {
    try {

      // Try to fail fast, before writing anything to disk.
      changes.foreach {
        case (f: File, s: String) => if (f.isDirectory || !f.canWrite) {
          throw new IllegalArgumentException("" + f + " is not a writable file.")
        }
        case _ =>
          throw new IllegalArgumentException("Invalid (File,String) pair.")
      }

      // Apply the changes. An error here may result in a corrupt disk state :(
      changes.foreach {
        case (file, newContents) =>
          replaceFileContents(file, newContents, cs) match {
            case Right(_) =>
            case Left(e) => Right(Left(e))
          }
      }

      Right(Right(()))

    } catch {
      case e: Exception => Left(e)
    }
  }
}
