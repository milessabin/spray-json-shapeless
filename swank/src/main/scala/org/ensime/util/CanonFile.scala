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
