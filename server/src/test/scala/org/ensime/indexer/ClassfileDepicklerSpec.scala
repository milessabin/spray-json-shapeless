package org.ensime.indexer

import org.scalatest.FunSpec
import org.scalatest.Matchers

class ClassfileDepicklerSpec extends FunSpec with Matchers with ClassfileDepickler {

  describe("ClassfileDepickler") {
    it("don't depickle J2SE classes") {
      assert(depickle(vres("java/lang/String.class")) === None)
    }

    it("support typical Scala classes") {
      assert(depickle(vres("scala/collection/immutable/List.class")).nonEmpty)
    }

    it("don't expect anything in companions") {
      assert(depickle(vres("scala/collection/immutable/List$.class")) === None)
    }

    it("don't expect anything in closures") {
      assert(depickle(vres("scala/util/matching/Regex$Groups$$anonfun$unapplySeq$4.class")) === None)
    }
  }
}
