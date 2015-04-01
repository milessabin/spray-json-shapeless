package org.ensime.sexp.formats

import org.ensime.sexp._

trait BasicFormats {

  implicit object UnitFormat extends SexpFormat[Unit] {
    def write(x: Unit) = SexpNil
    def read(value: Sexp) = ()
  }

  implicit object BooleanFormat extends SexpFormat[Boolean] {
    // all non-nil Sexps are technically "true"
    private val SexpTrue = SexpSymbol("t")
    def write(x: Boolean) = if (x) SexpTrue else SexpNil
    def read(value: Sexp) = value match {
      case SexpNil => false
      case _ => true
    }
  }

  implicit object CharFormat extends SexpFormat[Char] {
    def write(x: Char) = SexpChar(x)
    def read(value: Sexp) = value match {
      case SexpChar(x) => x
      case x => deserializationError(x)
    }
  }

  implicit object StringFormat extends SexpFormat[String] {
    def write(x: String) = SexpString(x)
    def read(value: Sexp) = value match {
      case SexpString(x) => x
      case x => deserializationError(x)
    }
  }

  // val allows override
  implicit val SymbolFormat = new SexpFormat[Symbol] {
    def write(x: Symbol): Sexp = SexpString(x.name)
    def read(value: Sexp): Symbol = value match {
      case SexpString(x) => Symbol(x)
      case x => deserializationError(x)
    }
  }

  /**
   * NOTE Emacs will not be able to correctly interpret arbitrary
   * precision numbers because - unlike other lisps - it uses a
   * reduced form of C double/integer precision. A round-trip via
   * Emacs for very large numbers will return `SexpPosInf`.
   *
   * The built-in Emacs library `'calc` has a few data formats
   * http://www.gnu.org/software/emacs/manual/html_mono/calc.html#Data-Type-Formats
   * but they fall short and require specific interpretation within
   * the `'calc` framework.
   *
   * If you need Emacs-specific support for arbitrary precision
   * numbers, override this implementation with one that adheres to
   * the arbitrary precision framework of your choice.
   */
  implicit def ViaBigDecimalFormat[T](implicit c: BigDecimalConvertor[T]) =
    new SexpFormat[T] {
      def write(x: T): Sexp =
        if (c.isNaN(x)) SexpNaN
        else if (c.isPosInf(x)) SexpPosInf
        else if (c.isNegInf(x)) SexpNegInf
        else SexpNumber(c.to(x))

      def read(value: Sexp): T = value match {
        case SexpNumber(x) => c.from(x)
        case SexpNaN => c.NaN
        case SexpPosInf => c.PosInf
        case SexpNegInf => c.NegInf
        case x => deserializationError(x)
      }
    }

  // boilerplate for performance (uses ViaBigDecimal)
  implicit val IntFormat = SexpFormat[Int]
  implicit val LongFormat = SexpFormat[Long]
  implicit val FloatFormat = SexpFormat[Float]
  implicit val DoubleFormat = SexpFormat[Double]
  implicit val ByteFormat = SexpFormat[Byte]
  implicit val ShortFormat = SexpFormat[Short]
  implicit val BigIntFormat = SexpFormat[BigInt]
  implicit val BigDecimalFormat = SexpFormat[BigDecimal]
}

trait SymbolAltFormat {
  this: BasicFormats =>
  override implicit val SymbolFormat = new SexpFormat[Symbol] {
    def write(x: Symbol): Sexp = SexpSymbol(x.name)
    def read(value: Sexp): Symbol = value match {
      case SexpSymbol(x) => Symbol(x)
      case x => deserializationError(x)
    }
  }
}
