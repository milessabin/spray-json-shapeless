package org.ensime.indexer.lucene

abstract class EntityS[T <: Entity](clazz: Class[T]) extends Serializer(clazz) {
  def id(t: T) = t.id
}