package org.ensime.test
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.ensime.util.{ SExpList, StringAtom, SExpParser, BooleanAtom }

class SExpSpec extends FunSpec with Matchers {

  describe("SExpSpec") {
    def check(input: String, expected: String) {
      val result = SExpParser.read(input).toString
      assert(result == expected, "expected " + expected + " got " + result)
      // throw it through again - everything should round trip perfectly
      val result2 = SExpParser.read(result).toString
      assert(result == result2, "round trip failed  " + result + " got " + result2)
    }

    it("should unapply BooleanAtom correctly") {
      SExpParser.read("t") match {
        case BooleanAtom(v) => assert(v, "result value should be true")
        case _ => fail("unapply failed")
      }

      SExpParser.read("nil") match {
        case BooleanAtom(v) => assert(!v, "result value should be false")
        case _ => fail("unapply failed")
      }

      SExpParser.read("""( "abc" )""") match {
        case BooleanAtom(v) => fail("unapply should not have succeeded")
        case _ =>
      }
    }

    it("should parse things right") {
      check("( 123 )", "(123)")
      check("()", "()")
      check("(nil)", "(nil)")
      check("(t)", "(t)")
      check("(a b c d)", "(a b c d)")
      check("(a b c () nil)", "(a b c () nil)")
      check("(a b c () trait)", "(a b c () trait)")
      check("(a b c () t())", "(a b c () t ())")
      check("(a b c\n() nil(nil\n t))", "(a b c () nil (nil t))")
      check("(nildude)", "(nildude)")
      check("""("\n")""", """("\n")""")
      check("""("\t")""", """("\t")""")
      check("""("\r")""", """("\r")""")
      check("\"a st\\\\ri\\\"n\\\\g\"", "\"a st\\\\ri\\\"n\\\\g\"")
      check("""("string  1"      "string  2")""", """("string  1" "string  2")""")
      assert(SExpParser.read("123").toScala == 123, "Int representation should be int")
      assert(SExpParser.read("t\n").toScala == true, "t should be true!")
      assert(SExpParser.read("t").asInstanceOf[BooleanAtom].toBool, "t should be true!")
      assert(SExpParser.read("t\n\t").toScala == true, "t should be true!")
      assert(SExpParser.read("t\n\t").toScala == true, "t should be true!")
      assert(SExpParser.read("t ").toScala == true, "t should be true!")
      assert(SExpParser.read("nil").toScala == false, "nil should be false!")
      assert(!SExpParser.read("nil").asInstanceOf[BooleanAtom].toBool, "t should be false!")
      assert(SExpParser.read("nil").toScala == false, "nil should be false!")
      assert(SExpParser.read("nil ").toScala == false, "nil should be false!")
      assert(SExpParser.read("nil\n").toScala == false, "nil should be false!")
      assert(SExpParser.read("nil\n\t").toScala == false, "nil should be false!")
      assert(SExpParser.read("nil\n\t").toScala == false, "nil should be false!")

      assert(SExpParser.read("""(1 "a" )""").toScala == List(1, "a"), "toScala on list should toScala children")

      assert(SExpParser.read("\"\\na\"").asInstanceOf[StringAtom].toScala == "\na", "scala value of String should be raw string")

    }
    it("toStringList should correctly convert SExpList(StringAtom*) to Some(List[String])") {
      val input1 = SExpParser.read("""("a" "b" "c")""").asInstanceOf[SExpList]
      assert(input1.toStringList == Some(List("a", "b", "c")))
    }

    it("should correctly convert SExpList(!StringAtom*) to None") {
      val input1 = SExpParser.read("""("a" 1 "c")""").asInstanceOf[SExpList]
      assert(input1.toStringList == None)
    }

    it("should parse long strings") {
      var l = List[Char]()
      for (i <- 1 to 100000) {
        l = 'x' :: l
      }
      val long = l.mkString
      val sexp = "\"" + long + "\""
      val expected = "\"" + long + "\""
      check(sexp, expected)
    }
  }
}
