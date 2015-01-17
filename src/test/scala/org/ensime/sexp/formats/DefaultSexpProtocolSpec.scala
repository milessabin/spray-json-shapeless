package org.ensime.sexp.formats

import org.ensime.sexp._

// these are a sort of aggregated test, investigating behaviours of
// interactions between the different formats.
class DefaultSexpProtocolSpec extends FormatSpec {

  describe("DefaultSexpProtocol") {
    import DefaultSexpProtocol._

    it("should support String as SexpString, not via IsTraversableLike") {
      assertFormat("hello", SexpString("hello"))
    }

    it("should round-trip Option[List[T]]") {
      val none: Option[List[String]] = None
      val empty: Option[List[String]] = Some(Nil)
      val list: Option[List[String]] = Some(List("boo"))

      assertFormat(none, SexpNil)
      assertFormat(empty, SexpList(SexpNil))
      assertFormat(list, SexpList(SexpList(SexpString("boo"))))
      assert(SexpNil.convertTo[Option[List[String]]] === None)
      assert(SexpNil.convertTo[List[Option[String]]] === Nil)
    }
  }

  describe("DefaultSexpProtocol with OptionAltFormat") {
    object AlternativeProtocol extends DefaultSexpProtocol with OptionAltFormat
    import AlternativeProtocol._

    it("should predictably fail to round-trip Option[List[T]]") {

      val none: Option[List[String]] = None
      val empty: Option[List[String]] = Some(Nil)
      val list: Option[List[String]] = Some(List("boo"))

      assert(none.toSexp === SexpNil)
      assert(empty.toSexp === SexpNil)
      assert(list.toSexp === SexpList(SexpString("boo")))

      assert(SexpNil.convertTo[Option[List[String]]] === None)

      // and Lists of Options give empty lists, muahahaha
      assert(SexpNil.convertTo[List[Option[String]]] === Nil)
    }

  }
}
