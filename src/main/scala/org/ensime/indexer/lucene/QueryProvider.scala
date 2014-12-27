package org.ensime.indexer.lucene

import org.apache.lucene.search.Query

trait QueryProvider[T] {
  def createQuery(t: T): Query
}