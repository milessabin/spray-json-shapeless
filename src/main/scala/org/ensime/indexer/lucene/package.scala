/**
 * To keep interaction with Lucene really simple, we make the
 * following assumptions about the entities that we index:
 *
 * 1. all entities are flat (no nested objects).
 *
 * 2. all values have a `String` representation.
 *
 * 3. field names are universal: e.g. a "name" field in one type of
 *    entity should be analyzed the same way as in another.
 *
 * 4. entities have a unique id that is derived from their content.
 *
 * which allows us to use case classes to define entities, getting us
 * serialisation and deserialisation with minimal boilerplate.
 * Field-based `Analyzer`s and `Query`s, on the other hand, can be
 * arbitrarily complex.
 *
 * In addition, Option[T]s are indexed but not stored (not
 * fully persistent).
 */
package org.ensime.indexer

import org.apache.lucene.document._
import org.apache.lucene.index.Term
import org.apache.lucene.search._
import org.apache.lucene.search.BooleanClause.Occur.MUST
import Field._

package object lucene {
  trait DocumentProvider[T] {
    def toDocument(t: T): Document
  }
  trait DocumentRecovery[T] {
    def toEntity(d: Document): T
  }
  trait QueryProvider[T] {
    def createQuery(t: T): Query
  }
  trait Entity extends Product {
    def id: String
  }

  implicit class RichEntity[T <: Entity](e: T) {
    def toDocument(implicit p: DocumentProvider[T]) = p.toDocument(e)
  }

  implicit class RichDocument(d: Document) {
    def toEntity[T](implicit p: DocumentRecovery[T]) = p.toEntity(d)
  }

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

  abstract class EntityS[T <: Entity](clazz: Class[T]) extends Serializer(clazz) {
    def id(t: T) = t.id
  }
}
