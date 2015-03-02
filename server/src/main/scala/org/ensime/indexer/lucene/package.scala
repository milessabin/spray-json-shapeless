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

package object lucene {
  implicit class RichEntity[T <: Entity](e: T) {
    def toDocument(implicit p: DocumentProvider[T]) = p.toDocument(e)
  }

  implicit class RichDocument(d: Document) {
    def toEntity[T](implicit p: DocumentRecovery[T]) = p.toEntity(d)
  }
}
