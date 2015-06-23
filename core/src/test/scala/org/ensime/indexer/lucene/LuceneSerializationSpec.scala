package org.ensime.indexer.lucene

import org.apache.lucene.document.Field._
import org.apache.lucene.document._
import org.scalatest.{ FunSpec, Matchers }

class LuceneSerializationSpec extends FunSpec with Matchers {

  def thereAndBackAgain[T](t: T)(implicit p: DocumentProvider[T], r: DocumentRecovery[T]): Unit = {
    val doc = p.toDocument(t)
    val back = r.toEntity(doc)
    assert(t === back)
  }

  case class SimpleThing(id: String, b: String) extends Entity
  implicit object SimpleThingS extends EntityS[SimpleThing](classOf[SimpleThing]) {
    def addFields(doc: Document, t: SimpleThing): Unit =
      doc.add(new TextField("b", t.b, Store.YES))
    def toEntity(doc: Document): SimpleThing =
      SimpleThing(doc.get("ID"), doc.get("b"))
  }

  describe("Lucene Entity Serialisation") {
    it("should serialise and deserialise a simple type") {
      val t = SimpleThing("hello", "world")
      thereAndBackAgain(t)
    }
  }

}
