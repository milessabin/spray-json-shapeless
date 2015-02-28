package org.ensime.indexer

import java.io.File
import akka.event.slf4j.SLF4JLogging
import org.apache.commons.vfs2._
import org.apache.commons.vfs2.impl._
import org.ensime.config._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import pimpathon.file._

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
    def watched(event: FileChangeEvent) = {
      val name = event.getFile.getName
      ClassfileSelector.include(name.getExtension)
    }

    def fileChanged(event: FileChangeEvent): Unit =
      if (watched(event))
        listeners foreach { list => Future { list.classfileChanged(event.getFile) } }
    def fileCreated(event: FileChangeEvent): Unit =
      if (watched(event))
        listeners foreach { list => Future { list.classfileAdded(event.getFile) } }
    def fileDeleted(event: FileChangeEvent): Unit =
      if (watched(event))
        listeners foreach { list => Future { list.classfileRemoved(event.getFile) } }
  })
  fm.setRecursive(true)
  fm.start()

  // WORKAROUND https://issues.apache.org/jira/browse/VFS-536
  // We don't have a dedicated test for this because it is an upstream bug
  private val workaround = new DefaultFileMonitor(new FileListener {
    private def targets =
      config.compileClasspath.filter(_.isDirectory).map(vfile).map(_.getName)
    def watched(event: FileChangeEvent) = {
      val dir = event.getFile
      val name = dir.getName
      targets.exists(name.isAncestor(_))
    }
    def fileChanged(event: FileChangeEvent): Unit =
      if (watched(event)) reset()
    def fileCreated(event: FileChangeEvent): Unit =
      if (watched(event)) reset()
    def fileDeleted(event: FileChangeEvent): Unit = {}
  })

  workaround.setRecursive(false)
  workaround.start()

  private def ancestors(f: File): List[File] = {
    val parent = f.getParentFile()
    if (parent == null) Nil
    else parent :: ancestors(parent)
  }

  // If directories are recreated, triggering the VFS-536 bug, we end up
  // calling reset() a lot of times. We should probably debounce it, but
  // it seems to happen so quickly that no damage is done.
  private def reset(): Unit = {
    log.info("Setting up new file watchers")
    // must remove then add to avoid leaks

    for {
      d <- config.targetClasspath
      _ = fm.removeFile(d)
      _ = fm.addFile(d)
      ancestor <- ancestors(d)
      if config.root.contains(ancestor)
      _ = workaround.removeFile(ancestor)
      _ = workaround.addFile(ancestor)
    } workaround.removeFile(config.root)

    workaround.addFile(config.root)
  }

  reset()

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
