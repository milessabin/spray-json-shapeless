package org.ensime

import java.io.File
import org.apache.commons.vfs2._

package object indexer {
  private[indexer] val vfs = VFS.getManager

  private[indexer] implicit def toFileObject(f: File): FileObject = vfile(f)

  private[indexer] def vfile(name: String) = vfs.resolveFile(name)
  private[indexer] def vfile(file: File) = vfs.toFileObject(file)
  private[indexer] def vres(path: String) = vfs.resolveFile("res:" + path)
  private[indexer] def vjar(jar: File) = vfs.resolveFile("jar:" + jar.getAbsolutePath)
  private[indexer] def vjar(jar: FileObject) = vfs.resolveFile("jar:" + jar.getName.getURI)

  private[indexer] abstract class RecursiveExtSelector extends FileSelector {
    def includeFile(info: FileSelectInfo): Boolean =
      include(info.getFile.getName.getExtension)
    def traverseDescendents(info: FileSelectInfo) = true
    def include: Set[String]
  }

  private[indexer] object ClassfileSelector extends RecursiveExtSelector {
    val include = Set("class")
  }

  private[indexer] object SourceSelector extends RecursiveExtSelector {
    val include = Set("scala", "java")
  }
}

