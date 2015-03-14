package org.ensime.indexer

import org.scalatest.FunSpec
import org.scalatest.Matchers

import pimpathon.file._

class ClassfileDepicklerSpec extends FunSpec with Matchers {

  describe("ClassfileDepickler") {
    it("don't depickle J2SE classes") {
      assert(new ClassfileDepickler(vres("java/lang/String.class")).scalasig === None)
    }

    it("support typical Scala classes") {
      assert(new ClassfileDepickler(vres("scala/collection/immutable/List.class")).scalasig.nonEmpty)
    }

    it("don't expect anything in companions") {
      assert(new ClassfileDepickler(vres("scala/collection/immutable/List$.class")).scalasig === None)
    }

    it("don't expect anything in closures") {
      assert(new ClassfileDepickler(vres("scala/util/matching/Regex$Groups$$anonfun$unapplySeq$4.class")).scalasig === None)
    }

    it("can find type aliases") {
      assert(new ClassfileDepickler(vres("scala/Predef.class")).getTypeAliases.contains(
        RawType("scala.Predef$String", Public)))
    }
  }
}
