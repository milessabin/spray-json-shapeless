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

  // // unlike case classes, include the type hint in the serialised form
  // // TODO: get this working as part of a family, rather than just standalone
  // implicit def singletonFormat[T <: Singleton](
  //   implicit w: Witness.Aux[T]) = new SexpFormat[T] {
  //   // TODO: scala names https://github.com/milessabin/shapeless/issues/256
  //   private val sexp = SexpSymbol(w.value.getClass.getName)
  //   private val value = w.value
  //   def write(t: T) = sexp
  //   def read(v: Sexp) =
  //     if (v == sexp) value
  //     else deserializationError(v)
  // }

  case class TypeHint[T](hint: SexpSymbol)
  implicit def typehint[T: TypeTag]: TypeHint[T] =
    TypeHint(SexpSymbol(":" + typeOf[T].dealias.toString.split("(\\.|\\$)").last))

  abstract class TraitFormat[T] extends SexpFormat[T] {
    protected def wrap[E](t: E)(implicit th: TypeHint[E], sf: SexpFormat[E]): Sexp =
      SexpData(th.hint -> t.toSexp)

    // implement by matching on the implementations and passing off to wrap
    // def write(t: T): Sexp

    final def read(sexp: Sexp): T = sexp match {
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
