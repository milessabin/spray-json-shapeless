package org.ensime.indexer

import akka.event.slf4j.SLF4JLogging
import org.apache.commons.vfs2._
import org.apache.commons.vfs2.impl._
import org.ensime.config._

trait ClassfileListener {
  def classfileAdded(f: FileObject): Unit
  def classfileRemoved(f: FileObject): Unit
  def classfileChanged(f: FileObject): Unit
}

trait SourceListener {
  def sourceAdded(f: FileObject): Unit
  def sourceRemoved(f: FileObject): Unit
  def sourceChanged(f: FileObject): Unit
}

/**
 * Watches the user's target output directories for classfiles that
 * need to be indexed or updated (i.e. picks up changes when the
 * compiler produces any output).
 *
 * If we were Java 7+ we'd be using
 * http://docs.oracle.com/javase/7/docs/api/java/nio/file/WatchService.html
 */
class ClassfileWatcher(
    config: EnsimeConfig,
    listeners: Seq[ClassfileListener]) extends SLF4JLogging {
  private val fm = new DefaultFileMonitor(new FileListener {
    def watched(event: FileChangeEvent) =
      ClassfileSelector.include(event.getFile.getName.getExtension)

    def fileChanged(event: FileChangeEvent): Unit =
      if (watched(event))
        listeners foreach (_.classfileChanged(event.getFile))
    def fileCreated(event: FileChangeEvent): Unit =
      if (watched(event))
        listeners foreach (_.classfileAdded(event.getFile))
    def fileDeleted(event: FileChangeEvent): Unit =
      if (watched(event))
        listeners foreach (_.classfileRemoved(event.getFile))
  })
  fm.setRecursive(true)
  fm.start()

  config.modules.values.foreach { m =>
    fm.addFile(m.target)
    fm.addFile(m.testTarget)
  }
}

class SourceWatcher(
    config: EnsimeConfig,
    listeners: Seq[SourceListener]) extends SLF4JLogging {
  private val fm = new DefaultFileMonitor(new FileListener {
    def watched(event: FileChangeEvent) =
      SourceSelector.include(event.getFile.getName.getExtension)

    def fileChanged(event: FileChangeEvent): Unit =
      if (watched(event))
        listeners foreach (_.sourceChanged(event.getFile))
    def fileCreated(event: FileChangeEvent): Unit =
      if (watched(event))
        listeners foreach (_.sourceAdded(event.getFile))
    def fileDeleted(event: FileChangeEvent): Unit =
      if (watched(event))
        listeners foreach (_.sourceRemoved(event.getFile))
  })
  fm.setRecursive(true)
  fm.start()

  config.modules.values.foreach { m =>
    m.sourceRoots foreach { r => fm.addFile(r) }
  }
}
