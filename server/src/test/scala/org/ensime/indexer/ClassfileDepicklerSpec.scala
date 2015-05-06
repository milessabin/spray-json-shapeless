package org.ensime.indexer

import org.scalatest.{ BeforeAndAfterAll, FunSpec, Matchers }

class ClassfileDepicklerSpec extends FunSpec with Matchers with BeforeAndAfterAll {

  var vfs: EnsimeVFS = _

  override def beforeAll(): Unit = {
    vfs = EnsimeVFS()
  }

  override def afterAll(): Unit = {
    vfs.close()
  }

  describe("ClassfileDepickler") {
    it("don't depickle J2SE classes") {
      assert(new ClassfileDepickler(vfs.vres("java/lang/String.class")).scalasig === None)
    }

    it("support typical Scala classes") {
      assert(new ClassfileDepickler(vfs.vres("scala/collection/immutable/List.class")).scalasig.nonEmpty)
    }

    it("don't expect anything in companions") {
      assert(new ClassfileDepickler(vfs.vres("scala/collection/immutable/List$.class")).scalasig === None)
    }

    it("don't expect anything in closures") {
      assert(new ClassfileDepickler(vfs.vres("scala/util/matching/Regex$Groups$$anonfun$unapplySeq$4.class")).scalasig === None)
    }

    it("can find type aliases") {
      assert(new ClassfileDepickler(vfs.vres("scala/Predef.class")).getTypeAliases.contains(
        RawType("scala.Predef$String", Public)
      ))
    }
  }
}
