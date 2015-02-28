package org.ensime.indexer

import org.apache.commons.vfs2.{ FileSelectInfo, FileSelector }

private[indexer] abstract class RecursiveExtSelector extends FileSelector {
  def includeFile(info: FileSelectInfo): Boolean =
    include(info.getFile.getName.getExtension)
  def traverseDescendents(info: FileSelectInfo) = true
  def include: Set[String]
}