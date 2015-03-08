package org.ensime.sexp

import com.google.common.base.Charsets
import com.google.common.io.Files
import org.scalatest._
import scala.util.Properties.userHome
import pimpathon.file._

class SexpParserSpec extends WordSpec with Matchers {
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

  "PimpedString" should {
    "use the parser" in {
      import org.ensime.sexp._
      "nil".parseSexp shouldBe SexpNil
    }
  }

  "Sexp Parser" should {
    "parse nil" in {
      parse("nil") shouldBe SexpNil
      parse("()") shouldBe SexpNil
      parse("( )") shouldBe SexpNil
      parse("(;blah\n)") shouldBe SexpNil
    }

    "parse lists of strings" in {
      parse("""("foo" "bar")""") shouldBe SexpList(foo, bar)
    }

    "parse escaped chars in strings" in {
      parse(""""z \\ \" \t \\t \\\t x\ x"""") shouldBe SexpString("z \\ \" \t \\t \\\t xx")

      parse(""""C:\\my\\folder"""") shouldBe SexpString("""C:\my\folder""")
    }

    "parse lists of chars" in {
      parse("""(?f ?b)""") shouldBe SexpList(SexpChar('f'), SexpChar('b'))
    }

    "parse lists of symbols" in {
      parse("(foo bar is?)") shouldBe SexpList(foosym, barsym, SexpSymbol("is?"))
    }

    "parse lists of numbers" in {
      parse("(1 -2 3.14 4e+16)") shouldBe SexpList(one, negtwo, pi, fourexp)
    }

    "parse NaN" in {
      parse("0.0e+NaN") shouldBe SexpNaN
      parse("-0.0e+NaN") shouldBe SexpNaN
    }

    "parse infinity" in {
      parse("1.0e+INF") shouldBe SexpPosInf
      parse("-1.0e+INF") shouldBe SexpNegInf
    }

    "parse lists within lists" in {
      parse("""((foo))""") shouldBe SexpList(SexpList(foosym))
      parse("""((foo) foo)""") shouldBe SexpList(SexpList(foosym), foosym)
    }

    "parse quoted expressions" in {
      parse("""'(:foo "foo" :bar "bar")""") shouldBe
        SexpCons(SexpSymbol("quote"), SexpList(fookey, foo, barkey, bar))

      parse("'foo") shouldBe SexpCons(SexpSymbol("quote"), foosym)
    }

    "parse cons" in {
      parse("(foo . bar)") shouldBe SexpCons(foosym, barsym)
    }

    "parse symbols with dots in their name" in {
      parse("foo.bar") shouldBe SexpSymbol("foo.bar")
      parse(":foo.bar") shouldBe SexpSymbol(":foo.bar")
    }
  }
}
