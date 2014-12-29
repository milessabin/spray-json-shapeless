package org.ensime.indexer.lucene

import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.{ Document, StringField }
import org.apache.lucene.index.Term
import org.apache.lucene.search.BooleanClause.Occur._
import org.apache.lucene.search.{ BooleanQuery, Query, TermQuery }

abstract class Serializer[T](clazz: Class[T])
    extends DocumentProvider[T] with DocumentRecovery[T] with QueryProvider[T] {
  private val TypeField = new StringField("TYPE", clazz.getSimpleName, Store.YES)
  private val TypeTerm = new TermQuery(new Term(TypeField.name, TypeField.stringValue))

  def id(t: T): String

  final def toDocument(t: T): Document = {
    val doc = new Document
    doc.add(TypeField)
    doc.add(new StringField("ID", id(t), Store.NO))
    addFields(doc, t)
    doc
  }
  def addFields(doc: Document, t: T): Unit

  final def createQuery(e: T): Query = {
    new BooleanQuery {
      add(TypeTerm, MUST)
      add(new TermQuery(new Term("ID", id(e))), MUST)
    }
  }
}

