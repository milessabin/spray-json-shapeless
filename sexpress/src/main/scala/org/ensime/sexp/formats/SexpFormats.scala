package org.ensime.sexp.formats

import org.ensime.sexp._
import scala.reflect.ClassTag

trait SexpFormats {
  /**
   * Constructs an `SexpFormat` from its two parts, `SexpReader` and `SexpWriter`.
   */
  def sexpFormat[T](reader: SexpReader[T], writer: SexpWriter[T]) = new SexpFormat[T] {
    def write(obj: T) = writer.write(obj)
    def read(json: Sexp) = reader.read(json)
  }

  implicit def sexpIdentityFormat[T <: Sexp: ClassTag] = new SexpFormat[T] {
    def write(o: T) = o
    def read(v: Sexp) = v match {
      case t: T => t
      case x => deserializationError(x)
    }
  }

  // performance boilerplate
  implicit val SexpFormat_ = SexpFormat[Sexp]
  implicit val SexpConsFormat = SexpFormat[SexpCons]
  implicit val SexpAtomFormat = SexpFormat[SexpAtom]
  implicit val SexpStringFormat = SexpFormat[SexpString]
  implicit val SexpNumberFormat = SexpFormat[SexpNumber]
  implicit val SexpCharFormat = SexpFormat[SexpChar]
  implicit val SexpSymbolFormat = SexpFormat[SexpSymbol]

}
