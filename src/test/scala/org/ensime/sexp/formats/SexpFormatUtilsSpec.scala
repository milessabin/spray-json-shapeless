package org.ensime.sexp.formats

import scala.util._

import org.ensime.sexp._

class SexpFormatUtilsSpec extends FormatSpec with SexpFormats {
  import SexpFormatUtils._

  describe("SexpFormatUtils") {
    it("should lift writers") {
      val lifted = lift(new SexpWriter[SexpString] {
        def write(o: SexpString) = o
      })
      assert(foo.toSexp(lifted) === foo)
      intercept[UnsupportedOperationException] {
        foo.convertTo[SexpString](lifted)
      }
    }

    it("should lift readers") {
      val lifted = lift(new SexpReader[SexpString] {
        def read(o: Sexp) = o.asInstanceOf[SexpString]
      })
      assert(foo.convertTo[SexpString](lifted) === foo)
      intercept[UnsupportedOperationException] {
        foo.toSexp(lifted)
      }
    }

    it("should combine readers and writers") {
      val reader = new SexpReader[SexpString] {
        def read(o: Sexp) = o.asInstanceOf[SexpString]
      }
      val writer = new SexpWriter[SexpString] {
        def write(o: SexpString) = o
      }
      val combo = sexpFormat(reader, writer)

      assert(foo.convertTo[SexpString](combo) === foo)
      assert(foo.toSexp(combo) === foo)
    }

    it("should support lazy formats") {
      var init = false
      val lazyF = lazyFormat {
        init = true
        SexpStringFormat
      }

      assert(!init)
      assert(SexpString("foo").convertTo[SexpString](lazyF) === SexpString("foo"))
      assert(init)
      assert(SexpString("foo").toSexp(lazyF) === SexpString("foo"))
    }

    it("should support safe readers") {
      val safe = safeReader(
        new SexpReader[SexpString] {
          def read(value: Sexp) = value match {
            case s: SexpString => s
            case x => deserializationError(x)
          }
        }
      )

      assert(foo.convertTo[Try[SexpString]](safe) === Success(foo))
      assert(bar.convertTo[Try[SexpString]](safe).isInstanceOf[Failure[_]])
    }
  }
}
