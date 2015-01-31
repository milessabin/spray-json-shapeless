package org.ensime.sexp.formats

import scala.reflect.runtime.universe._
import shapeless._

import org.ensime.sexp._

/**
 * Helper methods for generating wrappers for types in a family, also
 * known as "type hints".
 *
 * See https://gist.github.com/fommil/3a04661116c899056197
 *
 * Boilerplate blocked on https://github.com/milessabin/shapeless/issues/238
 */
trait FamilyFormats {
  case class TypeHint[T](hint: SexpSymbol)
  implicit def typehint[T: TypeTag]: TypeHint[T] =
    TypeHint(SexpSymbol(":" + typeOf[T].dealias.toString.replaceAll("\\.type$", "").split("(\\.|\\$)").last))

  // always serialises to Nil, and is differentiated by the TraitFormat
  //
  // this is not implicit because of a stray println in shapeless that
  // makes this extremely frustrating to use.
  // https://github.com/milessabin/shapeless/pull/259
  def singletonFormat[T <: Singleton](implicit w: Witness.Aux[T]) = new SexpFormat[T] {
    // TODO: scala names https://github.com/milessabin/shapeless/issues/256
    private val value = w.value
    def write(t: T) = SexpNil
    def read(v: Sexp) =
      if (v == SexpNil) value
      else deserializationError(v)
  }

  abstract class TraitFormat[T] extends SexpFormat[T] {
    protected def wrap[E](t: E)(implicit th: TypeHint[E], sf: SexpFormat[E]): Sexp = {
      val contents = t.toSexp
      // special cases: empty case clases, and case objects (hopefully)
      if (contents == SexpNil) SexpList(th.hint)
      else SexpData(th.hint -> contents)
    }

    // implement by matching on the implementations and passing off to wrap
    // def write(t: T): Sexp

    final def read(sexp: Sexp): T = sexp match {
      case SexpList(List(hint @ SexpSymbol(_))) => read(hint, SexpNil)
      case SexpData(map) if map.size == 1 =>
        map.head match {
          case (hint, value) => read(hint, value)
        }

      case x => deserializationError(x)
    }

    // implement by matching on the hint and passing off to convertTo[Impl]
    protected def read(hint: SexpSymbol, value: Sexp): T
  }
}
