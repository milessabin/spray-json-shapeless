package org.ensime.test
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.ensime.util.SExp


class SExpSpec extends Spec with ShouldMatchers{

  describe("SExpSpec") {
    it("should parse things right") {
      def check(s:String, r:String) {
	assert(SExp.read(s).toString() == r);
      }
      check("()", "()")
      check("(nil)", "(nil)")
      check("(t)", "(t)")
      check("(a b c d)", "(a b c d)")
      check("(a b c () nil)", "(a b c () nil)")
      check("(a b c () trait)", "(a b c () trait)")
      check("(a b c () t())", "(a b c () t ())")
      check("(a b c\n() nil(nil\n t))", "(a b c () nil (nil t))")
      check("(nildude)", "(nildude)")
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
  }

}
