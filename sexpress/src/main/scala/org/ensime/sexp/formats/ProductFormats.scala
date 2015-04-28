package org.ensime.sexp.formats

import org.ensime.sexp._
import shapeless._

trait LowPriorityProductFormats {

  /*
   Implementation note: this (and the `FamilyFormat`s) is likely to be
   replaced by a cleaner `(Labelled)TypeClass` implementation when
   shapeless 2.1 is released: it can handle the products and co-products
   at the same time (i.e. case classes, tuples, and sealed traits)
   */

  trait HListFormat[L <: HList] {
    def write(x: L): List[Sexp]
    def read(values: List[Sexp]): L
  }

  object HListFormat {
    implicit val HNilFormat: HListFormat[HNil] = new HListFormat[HNil] {
      def write(x: HNil) = Nil
      def read(value: List[Sexp]) = value match {
        case Nil => HNil
        case x => throw new DeserializationException(s"Didn't expect $x")
      }
    }

    implicit def hListFormat[H, T <: HList](
      implicit
      h: Lazy[SexpFormat[H]],
      t: Lazy[HListFormat[T]]
    ): HListFormat[H :: T] = new HListFormat[H :: T] {
      def write(x: H :: T) = h.value.write(x.head) :: t.value.write(x.tail)

      def read(values: List[Sexp]): H :: T = {
        import HList.ListCompat._
        values match {
          case head :: tail => h.value.read(head) :: t.value.read(tail)
          case x => throw new DeserializationException("Didn't expect Nil")
        }
      }
    }
  }

  /* We really want to have just `T` and its various representations,
   * e.g. `R <: Generic[T].Repr`, in the type parameters --- but
   * that's not possible due to limitations of the type system.
   *
   * Therefore we have to define the type very loosely (e.g. `R <:
   * HList`) and then restrict it by asking for an implicit `Aux`
   * which serves the purpose of the thing it is aux-ing. All the
   * implementations of the `Aux`s are provided by shapeless macros.
   */
  implicit def labelledProductFormat[T, R <: HList, LR <: HList, K <: HList](
    implicit
    g: Generic.Aux[T, R],
    lg: LabelledGeneric.Aux[T, LR],
    k: ops.record.Keys.Aux[LR, K],
    ltl: ops.hlist.ToList[K, Symbol],
    r: Lazy[HListFormat[R]]
  ): SexpFormat[T] = new SexpFormat[T] {

    private val keys = k().toList[Symbol].map { sym =>
      SexpSymbol(":" + toWireName(sym.name))
    }

    def write(x: T): Sexp =
      if (keys.isEmpty) SexpNil
      else SexpData(keys zip r.value.write(g.to(x)))

    def read(value: Sexp): T = value match {
      case SexpNil => g.from(r.value.read(Nil))
      case SexpData(pairs) =>
        val els = keys.map { k =>
          // missing keys are interpreted as nil
          pairs.getOrElse(k, SexpNil)
        }
        g.from(r.value.read(els))

      case x =>
        deserializationError(x)
    }
  }

  // capable of overloading for legacy formats
  def toWireName(field: String): String = field
}

trait ProductFormats extends LowPriorityProductFormats {
  // higher priority so that tuples and case classes are not ambiguous
  implicit def tupleProductFormat[T, R <: HList, T2](
    implicit
    g: Generic.Aux[T, R],
    t: ops.hlist.Tupler.Aux[R, T2],
    p: T =:= T2,
    r: Lazy[HListFormat[R]]
  ): SexpFormat[T] = new SexpFormat[T] {
    def write(x: T): Sexp = SexpList(r.value.write(g.to(x)))
    def read(value: Sexp): T = value match {
      case SexpList(els) => g.from(r.value.read(els))
      case x => deserializationError(x)
    }
  }
}

/**
 * By default, S-Express uses the same field names in the wire format
 * as the code. But some protocols may prefer dashes with Scala code
 * that uses camel case. This mix-in provides that behaviour.
 */
trait CamelCaseToDashes {
  this: LowPriorityProductFormats =>
  override def toWireName(field: String): String =
    field.replaceAll("([A-Z])", "-$1").toLowerCase.replaceAll("^-", "")
}
