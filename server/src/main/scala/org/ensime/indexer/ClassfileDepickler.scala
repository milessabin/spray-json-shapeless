package org.ensime.indexer

import com.google.common.io.ByteStreams
import org.apache.commons.vfs2.FileObject
import scala.tools.scalap.scalax.rules.scalasig._

trait ClassfileDepickler {
  /** Uses scalap to produce a scala reflective view of the classfile */
  def depickle(file: FileObject): Option[ScalaSig] = {
    val in = file.getContent.getInputStream
    try {
      val bytes = ByteStreams.toByteArray(in)
      val byteCode = ByteCode(bytes)
      val classFile = ClassFileParser.parse(byteCode)
      ScalaSigParser.parse(classFile)
    } finally in.close()
  }
}
