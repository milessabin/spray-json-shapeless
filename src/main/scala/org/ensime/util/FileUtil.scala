package org.ensime.util

import java.io._
import java.net.URI
import java.nio.charset.Charset
import org.apache.commons.vfs2.FileObject
import scala.collection.Seq
import scala.collection.mutable
import scala.util.Try
import scala.sys.process._

import pimpathon.file._

/** A wrapper around file, allowing iteration either on direct children or on directory tree */
class RichFile(file: File) {

  def canon: File =
    try file.getCanonicalFile
    catch {
      case e: Exception => file.getAbsoluteFile
    }
}

/** implicitly enrich java.io.File with methods of RichFile */
object RichFile {
  implicit def toRichFile(file: File): RichFile = new RichFile(file)
}

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

class CanonFile private (path: String) extends File(path)

object CanonFile {
  def apply(file: File): CanonFile = {
    try {
      new CanonFile(file.getCanonicalPath)
    } catch {
      case e: Exception => new CanonFile(file.getAbsolutePath)
    }
  }

  def apply(str: String): CanonFile = apply(new File(str))
}

object FileUtils {

  // WORKAROUND: https://github.com/typelevel/scala/issues/75
  val jdkDir: File = List(
    // manual
    sys.env.get("JDK_HOME"),
    sys.env.get("JAVA_HOME"),
    // osx
    Try("/usr/libexec/java_home".!!.trim).toOption,
    // fallback
    sys.props.get("java.home").map(new File(_).getParent),
    sys.props.get("java.home")
  ).flatten.find(n =>
      new File(n + "/lib/tools.jar").exists).map(new File(_)).getOrElse(
      throw new FileNotFoundException(
        """Could not automatically find the JDK/lib/tools.jar.
      |You must explicitly set JDK_HOME or JAVA_HOME.""".stripMargin
      )
    )

  implicit def toCanonFile(file: File): CanonFile = CanonFile(file)

  def isScalaSourceFile(f: File): Boolean = {
    f.exists && f.getName.endsWith(".scala")
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
      val writer = new FileWriter(file)
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
    editsByFile.foreach {
      case (file, fileEdits) =>
        readFile(file) match {
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

  def writeChanges(changes: Iterable[FileEdit]): Either[Exception, Iterable[File]] = {
    val editsByFile = changes.collect { case ed: TextEdit => ed }.groupBy(_.file)
    val newFiles = changes.collect { case ed: NewFile => ed }
    try {
      val rewriteList = newFiles.map { ed => (ed.file, ed.text) } ++
        editsByFile.map {
          case (file, fileChanges) =>
            readFile(file) match {
              case Right(contents) =>
                val newContents = FileEdit.applyEdits(fileChanges.toList, contents)
                (file, newContents)
              case Left(e) => throw e
            }
        }

      val deleteFiles = changes.collect { case ed: DeleteFile => ed }
      for (ed <- deleteFiles) {
        ed.file.delete()
      }

      rewriteFiles(rewriteList) match {
        case Right(Right(_)) => Right(changes.groupBy(_.file).keys)
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
    try {

      // Try to fail fast, before writing anything to disk.
      changes.foreach {
        case (f: File, s: String) => if (f.isDirectory || !f.canWrite) {
          throw new IllegalArgumentException(f + " is not a writable file.")
        }
        case _ =>
          throw new IllegalArgumentException("Invalid (File,String) pair.")
      }

      // Apply the changes. An error here may result in a corrupt disk state :(
      changes.foreach {
        case (file, newContents) =>
          replaceFileContents(file, newContents) match {
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
