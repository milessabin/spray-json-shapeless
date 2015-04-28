package org.ensime.sexp.formats

import org.ensime.sexp._

class ProductFormatsSpec extends FormatSpec
    with BasicFormats with StandardFormats with ProductFormats {

  case class Foo(i: Int, s: String)
  case class Bar(foo: Foo)
  case class Baz()
  case class Wibble(thing: String, thong: Int, bling: Option[String])

  describe("ProductFormats case classes") {
    val foo = Foo(13, "foo")
    val fooexpect = SexpData(
      SexpSymbol(":i") -> SexpNumber(13),
      SexpSymbol(":s") -> SexpString("foo")
    )

    it("should support primitive types") {
      // will create the marshaller every time assertFormat is called
      assertFormat(foo, fooexpect)
      assertFormat(foo, fooexpect)
      assertFormat(foo, fooexpect)
    }

    it("should support 'fast' case classes") {
      // can't really test - its a side effect optimisation
      implicit val FastFooFormat = SexpFormat[Foo]
      assertFormat(foo, fooexpect)
      assertFormat(foo, fooexpect)
      assertFormat(foo, fooexpect)
    }

    it("should support nested case classes") {
      val bar = Bar(foo)
      val expect = SexpData(
        SexpSymbol(":foo") -> fooexpect
      )

      // (this is actually a really big deal, thank you shapeless!)
      assertFormat(bar, expect)
    }

    it("should support zero content case classes") {
      assertFormat(Baz(), SexpNil)
    }

    it("should support missing fields as SexpNil / None") {
      val wibble = Wibble("wibble", 13, Some("fork"))

      assertFormat(wibble, SexpData(
        SexpSymbol(":thing") -> SexpString("wibble"),
        SexpSymbol(":thong") -> SexpNumber(13),
        SexpSymbol(":bling") -> SexpList(SexpString("fork"))
      ))

      val wobble = Wibble("wibble", 13, None)

      // write out None as SexpNil
      assertFormat(wobble, SexpData(
        SexpSymbol(":thing") -> SexpString("wibble"),
        SexpSymbol(":thong") -> SexpNumber(13),
        SexpSymbol(":bling") -> SexpNil
      ))

      // but tolerate missing entries
      assert(SexpData(
        SexpSymbol(":thing") -> SexpString("wibble"),
        SexpSymbol(":thong") -> SexpNumber(13)
      ).convertTo[Wibble] === wobble)
    }
  }

  describe("ProductFormat tuples") {
    val foo = (13, "foo")
    val fooexpect = SexpList(SexpNumber(13), SexpString("foo"))

    it("should support primitive types") {
      assertFormat(foo, fooexpect)
    }

    it("should support 'fast' tuples") {
      // can't really test - its a side effect optimisation
      implicit val FastFooFormat = SexpFormat[(Int, String)]
      assertFormat(foo, fooexpect)
      assertFormat(foo, fooexpect)
      assertFormat(foo, fooexpect)
    }

  }
}

class CustomisedProductFormatsSpec extends FormatSpec
    with BasicFormats with StandardFormats with ProductFormats
    with CamelCaseToDashes {

  case class Foo(AThingyMaBob: Int, HTML: String)

  describe("ProductFormats with overloaded toWireName") {
    it("should support custom field names") {
      assertFormat(Foo(13, "foo"), SexpData(
        SexpSymbol(":a-thingy-ma-bob") -> SexpNumber(13),
        SexpSymbol(":h-t-m-l") -> SexpString("foo")
      ))
    }
  }
}
