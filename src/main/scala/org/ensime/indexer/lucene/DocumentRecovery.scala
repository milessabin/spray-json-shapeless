package org.ensime.indexer.lucene

import org.apache.lucene.document.Document

trait DocumentRecovery[T] {
  def toEntity(d: Document): T
}

