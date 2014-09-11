package org.ensime.indexer

import akka.event.slf4j.SLF4JLogging
import org.apache.commons.vfs2._
import org.ensime.config._
import pimpathon.list._
import pimpathon.map._

// mutable: lookup of user's source files are atomically updated
class SourceResolver(config: EnsimeConfig) extends SourceListener with SLF4JLogging {

  // it's not worth doing incremental updates - this is cheap
  // (but it would be nice to have a "debounce" throttler)
  def sourceAdded(f: FileObject) = update()
  def sourceRemoved(f: FileObject) = update()
  def sourceChanged(f: FileObject) = {}

  // we only support the case where RawSource has a Some(filename)
  def resolve(clazz: PackageName, source: RawSource): Option[FileObject] =
    source.filename match {
      case None => None
      case Some(filename) => all.get(clazz) flatMap {
        _.find(_.getName.getBaseName == filename)
      } match {
        case s @ Some(_) => s
        case None if clazz.path == Nil => None
        case _ => resolve(clazz.parent, source)
      }
    }

  def update(): Unit = {
    log.debug("updating sources")
    all = recalculate
  }

  private def scan(f: FileObject) = f.findFiles(SourceSelector) match {
    case null => Nil
    case res => res.toList
  }

  private val depSources = {
    val srcJars = config.referenceSourceJars.toSet ++ {
      for {
        (_, module) <- config.modules
        srcArchive <- module.referenceSourcesJars
      } yield srcArchive
    }
    for {
      srcJarFile <- srcJars
      // interestingly, this is able to handle zip files
      srcJar = vjar(srcJarFile)
      srcEntry <- scan(srcJar)
    } yield (infer(srcJar, srcEntry), srcEntry)
  }.toMultiMap[Set]

  private def userSources = {
    for {
      (_, module) <- config.modules.toList
      root <- module.sourceRoots
      dir = vfile(root)
      file <- scan(dir)
    } yield (infer(dir, file), file)
  }.toMultiMap[Set]

  private def recalculate = depSources merge userSources

  private var all = recalculate

  private def infer(base: FileObject, file: FileObject): PackageName = {
    // getRelativeName feels the wrong way round, but this is correct
    val relative = base.getName.getRelativeName(file.getName)
    // vfs separator char is always /
    PackageName((relative split "/").toList.init)
  }

}

