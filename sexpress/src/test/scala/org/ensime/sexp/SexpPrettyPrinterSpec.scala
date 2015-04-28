package org.ensime.sexp

import org.scalatest.FunSpec

class SexpPrettyPrinterSpec extends FunSpec {

  private val foo = SexpString("foo")
  private val foosym = SexpSymbol("foo")
  private val barsym = SexpSymbol("bar")
  private val fookey = SexpSymbol(":foo")
  private val barkey = SexpSymbol(":bar")
  private def assertPrinter(sexp: Sexp, expect: String): Unit = {
    //    println("GOT\n" + SexpPrettyPrinter(sexp))
    //    println("EXPECT\n" + expect)
    assert(SexpPrettyPrinter(sexp) === expect.replace("\r", ""))
  }

  describe("CompactPrinter") {
    it("should handle nil or empty lists/data") {
      assertPrinter(SexpNil, "nil")
      assertPrinter(SexpList(Nil), "nil")
    }

    it("should output lists of atoms") {
      assertPrinter(
        SexpList(foo, SexpNumber(13), foosym),
        """("foo"
          |  13
          |  foo)""".stripMargin
      )
    }

    it("should output lists of lists") {
      assertPrinter(
        SexpList(SexpList(foo), SexpList(foo)),
        """(("foo")
          |  ("foo"))""".stripMargin
      )
    }

    it("should output data") {
      assertPrinter(
        SexpData(fookey -> foosym, barkey -> foosym),
        """(
  :foo foo
  :bar foo
)"""
      )

      val datum = SexpData(fookey -> foo, barkey -> foo)
      assertPrinter(SexpData(
        fookey -> datum,
        barkey -> datum
      ), """(
  :foo (
    :foo "foo"
    :bar "foo"
  )
  :bar (
    :foo "foo"
    :bar "foo"
  )
)""")
    }

    it("should output cons") {
      assertPrinter(SexpCons(foosym, barsym), "(foo .\n  bar)")
    }

  }
}
