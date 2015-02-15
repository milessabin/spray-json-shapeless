package org.ensime.sexp

import org.scalatest.FunSpec

class SexpCompactPrinterSpec extends FunSpec {

  private val foo = SexpString("foo")
  private val foosym = SexpSymbol("foo")
  private val barsym = SexpSymbol("bar")
  private def assertPrinter(sexp: Sexp, expect: String): Unit = {
    assert(SexpCompactPrinter(sexp) === expect)
  }

  describe("CompactPrinter") {
    it("should handle nil or empty lists/data") {
      assertPrinter(SexpNil, "nil")
      assertPrinter(SexpList(Nil), "nil")
    }

    it("should output lists of atoms") {
      assertPrinter(SexpList(foo, SexpNumber(13), foosym), """("foo" 13 foo)""")
    }

    it("should output lists of lists") {
      assertPrinter(SexpList(SexpList(foo), SexpList(foo)), """(("foo") ("foo"))""")
    }

    it("should output cons") {
      assertPrinter(SexpCons(foosym, barsym), "(foo . bar)")
    }

    it("should output escaped characters") {
      assertPrinter(SexpString("""C:\my\folder"""), """"C:\\my\\folder"""")
    }

  }
}
