package org.ensime.indexer

import java.io.File

import org.apache.commons.vfs2.FileObject
import org.apache.commons.vfs2.impl.{ StandardFileSystemManager, DefaultFileSystemManager }

object EnsimeVFS {
  def apply(): EnsimeVFS = {
    val vfsInst = new StandardFileSystemManager()
    vfsInst.init()
    new EnsimeVFS(vfsInst)
  }

  private[indexer] object ClassfileSelector extends RecursiveExtSelector {
    val include = Set("class")
  }

  private[indexer] object SourceSelector extends RecursiveExtSelector {
    val include = Set("scala", "java")
  }
}

class EnsimeVFS(val vfs: DefaultFileSystemManager) {

  private[indexer] implicit def toFileObject(f: File): FileObject = vfile(f)

  private[indexer] def vfile(name: String) = vfs.resolveFile(name)
  private[indexer] def vfile(file: File) = vfs.toFileObject(file)
  private[indexer] def vres(path: String) = vfs.resolveFile("res:" + path)
  private[indexer] def vjar(jar: File) = vfs.resolveFile("jar:" + jar.getAbsolutePath)
  private[indexer] def vjar(jar: FileObject) = vfs.resolveFile("jar:" + jar.getName.getURI)

  def close(): Unit = {
    vfs.close()
  }
}
