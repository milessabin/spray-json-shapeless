package org.ensime.sexp

import com.google.common.base.Charsets
import com.google.common.io.Files
import org.scalatest.FunSpec
import org.scalatest.Matchers
import scala.util.Properties.userHome
import pimpathon.file._

class SexpParserSpec extends FunSpec with Matchers {
  import SexpParser.{ parse, flatParse }

  val foo = SexpString("foo")
  val bar = SexpString("bar")
  val one = SexpNumber(1)
  val negtwo = SexpNumber(-2)
  val pi = SexpNumber("3.14")
  val fourexp = SexpNumber("4e+16")
  val foosym = SexpSymbol("foo")
  val barsym = SexpSymbol("bar")
  val fookey = SexpSymbol(":foo")
  val barkey = SexpSymbol(":bar")

  describe("PimpedString") {
    it("should use the parser") {
      import org.ensime.sexp._
      assert("nil".parseSexp === SexpNil)
    }
  }

  describe("Sexp Parser") {
    ignore("should parse my .emacs") {
      // a good source of corner cases!
      val dotemacs = file(userHome + "/.emacs")
      val contents = Files.toString(dotemacs, Charsets.UTF_8)
      println(flatParse(contents).prettyPrint)
      //println(flatParse(contents).compactPrint)
      //println(flatParse(contents).toBasic)
    }

    it("should parse nil") {
      assert(parse("nil") === SexpNil)
      assert(parse("()") === SexpNil)
      assert(parse("( )") === SexpNil)
      assert(parse("(;blah\n)") === SexpNil)
    }

    it("should parse lists of strings") {
      assert(parse("""("foo" "bar")""") === SexpList(foo, bar))
    }

    it("should parse escaped chars in strings") {
      assert(parse(""""z \\ \" \t \\t \\\t x\ x"""") === SexpString("z \\ \" \t \\t \\\t xx"))

      assert(parse(""""C:\\my\\folder"""") === SexpString("""C:\my\folder"""))
    }

    it("should parse lists of chars") {
      assert(parse("""(?f ?b)""") === SexpList(SexpChar('f'), SexpChar('b')))
    }

    it("should parse lists of symbols") {
      assert(parse("(foo bar is?)") === SexpList(foosym, barsym, SexpSymbol("is?")))
    }

    it("should parse lists of numbers") {
      assert(parse("(1 -2 3.14 4e+16)") === SexpList(one, negtwo, pi, fourexp))
    }

    it("should parse NaN") {
      assert(parse("0.0e+NaN") === SexpNaN)
      assert(parse("-0.0e+NaN") === SexpNaN)
    }

    it("should parse infinity") {
      assert(parse("1.0e+INF") === SexpPosInf)
      assert(parse("-1.0e+INF") === SexpNegInf)
    }

    it("should parse lists within lists") {
      assert(parse("""((foo))""") === SexpList(SexpList(foosym)))
      assert(parse("""((foo) foo)""") === SexpList(SexpList(foosym), foosym))
    }

    it("should parse quoted expressions") {
      assert(parse("""'(:foo "foo" :bar "bar")""") ===
        SexpCons(SexpSymbol("quote"), SexpList(fookey, foo, barkey, bar)))

      assert(parse("'foo") === SexpCons(SexpSymbol("quote"), foosym))
    }

    it("should parse cons") {
      assert(parse("(foo . bar)") === SexpCons(foosym, barsym))
    }

    it("should parse symbols with dots in their name") {
      assert(parse("foo.bar") === SexpSymbol("foo.bar"))
      assert(parse(":foo.bar") === SexpSymbol(":foo.bar"))
    }
  }
}
