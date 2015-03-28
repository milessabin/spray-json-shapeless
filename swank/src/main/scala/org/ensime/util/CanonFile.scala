package org.ensime.util

import java.io.File

class CanonFile private (path: String) {
  val file = new File(path)
}

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
