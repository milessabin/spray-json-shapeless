
package org.ensime.sexp.formats

import collection.{ immutable => im }

import org.ensime.sexp._

// http://docs.scala-lang.org/overviews/collections/overview.html
class CollectionFormatsSpec extends FormatSpec
    with ProductFormats with CollectionFormats with BasicFormats {

  val foos: List[String] = List("foo", "foo")
  val expect = SexpList(foo, foo)

  describe("CollectionFormats traits") {
    it("should support Traversable") {
      assertFormat(collection.Traversable[String](), SexpNil)
      assertFormat(collection.Traversable(foos: _*), expect)
    }

    it("should support Iterable") {
      assertFormat(collection.Iterable[String](), SexpNil)
      assertFormat(collection.Iterable(foos: _*), expect)
    }

    it("should support Seq") {
      assertFormat(collection.Seq[String](), SexpNil)
      assertFormat(collection.Seq(foos: _*), expect)
    }

    it("should support IndexedSeq") {
      assertFormat(collection.IndexedSeq[String](), SexpNil)
      assertFormat(collection.IndexedSeq(foos: _*), expect)
    }

    it("should support LinearSeq") {
      assertFormat(collection.LinearSeq[String](), SexpNil)
      assertFormat(collection.LinearSeq(foos: _*), expect)
    }

    it("should support Set") {
      assertFormat(collection.Set[String](), SexpNil)
      assertFormat(collection.Set(foos: _*), SexpList(foo)) // dupes removed
    }

    it("should support SortedSet") {
      assertFormat(collection.SortedSet[String](), SexpNil)
      assertFormat(collection.SortedSet(foos: _*), SexpList(foo)) // dupes removed
    }

    it("should support BitSet") {
      assertFormat(collection.BitSet(), SexpNil)
      assertFormat(collection.BitSet(0, 1), SexpString("16#3"))
      assertFormat(collection.BitSet(64), SexpString("16#10000000000000000"))
      assertFormat(collection.BitSet(0, 64), SexpString("16#10000000000000001"))
      assertFormat(collection.BitSet(1, 64), SexpString("16#10000000000000002"))
    }

    it("should support Map") {
      assertFormat(collection.Map[String, String](), SexpNil)
      assertFormat(collection.Map("foo" -> "foo"), SexpList(SexpList(foo, foo)))
    }

    it("should support SortedMap") {
      assertFormat(collection.SortedMap[String, String](), SexpNil)
      assertFormat(collection.SortedMap("foo" -> "foo"), SexpList(SexpList(foo, foo)))
    }
  }

  describe("CollectionFormats immutable variants of the traits") {
    it("should support Traversable") {
      assertFormat(im.Traversable[String](), SexpNil)
      assertFormat(im.Traversable(foos: _*), expect)
    }

    it("should support Iterable") {
      assertFormat(im.Iterable[String](), SexpNil)
      assertFormat(im.Iterable(foos: _*), expect)
    }

    it("should support Seq") {
      assertFormat(im.Seq[String](), SexpNil)
      assertFormat(im.Seq(foos: _*), expect)
    }

    it("should support IndexedSeq") {
      assertFormat(im.IndexedSeq[String](), SexpNil)
      assertFormat(im.IndexedSeq(foos: _*), expect)
    }

    it("should support LinearSeq") {
      assertFormat(im.LinearSeq[String](), SexpNil)
      assertFormat(im.LinearSeq(foos: _*), expect)
    }

    it("should support Set") {
      assertFormat(im.Set[String](), SexpNil)
      assertFormat(im.Set(foos: _*), SexpList(foo)) // dupes removed
    }

    it("should support SortedSet") {
      assertFormat(im.SortedSet[String](), SexpNil)
      assertFormat(im.SortedSet(foos: _*), SexpList(foo)) // dupes removed
    }

    it("should support BitSet") {
      assertFormat(im.BitSet(), SexpNil)
      assertFormat(im.BitSet(0, 1), SexpString("16#3"))
      assertFormat(collection.BitSet(64), SexpString("16#10000000000000000"))
      assertFormat(collection.BitSet(0, 64), SexpString("16#10000000000000001"))
      assertFormat(collection.BitSet(1, 64), SexpString("16#10000000000000002"))
    }

    it("should support Map") {
      assertFormat(im.Map[String, String](), SexpNil)
      assertFormat(im.Map("foo" -> "foo"), SexpList(SexpList(foo, foo)))
    }

    it("should support SortedMap") {
      assertFormat(im.SortedMap[String, String](), SexpNil)
      assertFormat(im.SortedMap("foo" -> "foo"), SexpList(SexpList(foo, foo)))
    }
  }

  describe("CollectionFormats immutable specific implementations") {
    it("should support im.List") {
      assertFormat(im.List[String](), SexpNil)
      assertFormat(im.List(foos: _*), expect)
    }

    it("should support im.Vector") {
      assertFormat(im.Vector[String](), SexpNil)
      assertFormat(im.Vector(foos: _*), expect)
    }

    it("should support im.Range") {
      assertFormat(
        im.Range(-100, 100),
        SexpList(
          SexpSymbol(":start"), SexpNumber(-100),
          SexpSymbol(":end"), SexpNumber(100),
          SexpSymbol(":step"), SexpNumber(1)
        )
      )

      assertFormat(
        im.Range(-100, 100, 2),
        SexpList(
          SexpSymbol(":start"), SexpNumber(-100),
          SexpSymbol(":end"), SexpNumber(100),
          SexpSymbol(":step"), SexpNumber(2)
        )
      )
    }

    it("should support im.NumericRange") {
      implicit val DoubleIntegral = Numeric.DoubleAsIfIntegral

      assertFormat(
        -100.0 to 100.0 by 1.5,
        SexpData(
          SexpSymbol(":start") -> SexpNumber(-100),
          SexpSymbol(":end") -> SexpNumber(100),
          SexpSymbol(":step") -> SexpNumber(1.5),
          SexpSymbol(":inclusive") -> SexpSymbol("t")
        )
      )

      assertFormat(
        -100.0 until 100.0 by 1.5,
        SexpData(
          SexpSymbol(":start") -> SexpNumber(-100),
          SexpSymbol(":end") -> SexpNumber(100),
          SexpSymbol(":step") -> SexpNumber(1.5),
          SexpSymbol(":inclusive") -> SexpNil
        )
      )
    }

  }
}
