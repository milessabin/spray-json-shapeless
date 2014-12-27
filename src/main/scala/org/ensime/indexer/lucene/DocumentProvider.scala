package org.ensime.indexer.lucene

import org.apache.lucene.document.Document

trait DocumentProvider[T] {
  def toDocument(t: T): Document
}