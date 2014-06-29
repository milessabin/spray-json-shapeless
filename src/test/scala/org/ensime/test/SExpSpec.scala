package org.ensime.test
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.ensime.util.SExp

class SExpSpec extends FunSpec with ShouldMatchers {

  describe("SExpSpec") {
    def check(s: String, r: String) {
      val res = SExp.read(s).toString()
      assert(res == r, "expected " + r + " got " + res)
    }

    it("should parse things right") {
      check("()", "()")
      check("(nil)", "(nil)")
      check("(t)", "(t)")
      check("(a b c d)", "(a b c d)")
      check("(a b c () nil)", "(a b c () nil)")
      check("(a b c () trait)", "(a b c () trait)")
      check("(a b c () t())", "(a b c () t ())")
      check("(a b c\n() nil(nil\n t))", "(a b c () nil (nil t))")
      check("(nildude)", "(nildude)")
      check("\"a st\\\\ri\\\"n\\g\"", "a st\\ri\"n\\g")
      check("(\"string  1\"      \"string  2\")", "(string  1 string  2)")
      assert(SExp.read("t\n").toScala == true, "t should be true!")
      assert(SExp.read("t\n\t").toScala == true, "t should be true!")
      assert(SExp.read("t\n\t").toScala == true, "t should be true!")
      assert(SExp.read("t ").toScala == true, "t should be true!")
      assert(SExp.read("t").toScala == true, "t should be true!")
      assert(SExp.read("nil\n").toScala == false, "nil should be false!")
      assert(SExp.read("nil\n\t").toScala == false, "nil should be false!")
      assert(SExp.read("nil\n\t").toScala == false, "nil should be false!")
      assert(SExp.read("nil ").toScala == false, "nil should be false!")
      assert(SExp.read("nil").toScala == false, "nil should be false!")
    }

    it("should parse long strings") {
      var l = List[Char]()
      for (i <- 1 to 100000) {
        l = 'x' :: l
      }
      val long = l.mkString
      val sexp = "\"" + long + "\""
      val expected = long
      check(sexp, expected)
    }
  }
}
