package org.ensime.sexp

import collection.breakOut
import scala.collection.immutable.ListMap

/**
 * An S-Expression is either
 *
 * 1. an atom (i.e. symbol, string, number)
 * 2. of the form `(x . y)` where `x` and `y` are S-Expressions (i.e. cons)
 *
 * Everything else is just sugar.
 */
sealed abstract class Sexp {
  //  override def toString = compactPrint
  def compactPrint = SexpCompactPrinter(this)
  def prettyPrint = SexpPrettyPrinter(this)

  def convertTo[T](implicit reader: SexpReader[T]): T = reader.read(this)

  private[sexp] def isList: Boolean = false
}

case class SexpCons(x: Sexp, y: Sexp) extends Sexp {
  private[sexp] override val isList = y.isList
}

sealed trait SexpAtom extends Sexp
case class SexpChar(value: Char) extends SexpAtom
case class SexpString(value: String) extends SexpAtom
case class SexpNumber(value: BigDecimal) extends SexpAtom
case class SexpSymbol(value: String) extends SexpAtom
case object SexpNil extends SexpAtom {
  private[sexp] override def isList = true
}
// https://www.gnu.org/software/emacs/manual/html_node/elisp/Float-Basics.html
case object SexpPosInf extends SexpAtom
case object SexpNegInf extends SexpAtom
case object SexpNaN extends SexpAtom

object SexpNumber {
  def apply(n: Int) = new SexpNumber(BigDecimal(n))
  def apply(n: Long) = new SexpNumber(BigDecimal(n))
  def apply(n: Double) = n match {
    case _ if n.isNaN => SexpNil
    case _ if n.isInfinity => SexpNil
    case _ => new SexpNumber(BigDecimal(n))
  }
  def apply(n: BigInt) = new SexpNumber(BigDecimal(n))
  def apply(n: String) = new SexpNumber(BigDecimal(n))
  def apply(n: Array[Char]) = new SexpNumber(BigDecimal(n))
}

/** Sugar for ("a" . ("b" . ("c" . nil))) */
object SexpList {
  def apply(els: Sexp*): Sexp = apply(els.toList)

  def apply(els: List[Sexp]): Sexp = els match {
    case Nil => SexpNil
    case head :: tail => SexpCons(head, apply(tail))
  }

  def unapply(sexp: Sexp): Option[List[Sexp]] =
    if (!sexp.isList) None
    else {
      def rec(s: Sexp): List[Sexp] = s match {
        case SexpNil => Nil
        case SexpCons(car, cdr) => car :: rec(cdr)
        case _ => throw new IllegalStateException("Not a list: " + s)
      }
      val res = rec(sexp)
      if (res.isEmpty) None
      else Some(res)
    }
}

/**
 * Sugar for (:k1 v1 :k2 v2)
 * [keyword symbols](https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Type.html):
 */
object SexpData {
  def apply(kvs: (SexpSymbol, Sexp)*): Sexp = apply(kvs.toList)

  def apply(kvs: List[(SexpSymbol, Sexp)]): Sexp =
    if (kvs.isEmpty)
      SexpNil
    else {
      val mapped = kvs.toMap
      require(mapped.size == kvs.size, "duplicate keys not allowed: " + mapped.keys)
      require(mapped.keys.forall(_.value.startsWith(":")), "keys must start with ':' " + mapped.keys)
      SexpList(kvs.flatMap { case (k, v) => k :: v :: Nil }(breakOut): List[Sexp])
    }

  def unapply(sexp: Sexp): Option[Map[SexpSymbol, Sexp]] = sexp match {
    case SexpList(values) =>
      // order can be important in serialised forms
      val props = {
        values.grouped(2).collect {
          case List(SexpSymbol(key), value) if key.startsWith(":") =>
            (SexpSymbol(key), value)
        }
      }.foldLeft(ListMap.empty[SexpSymbol, Sexp]) {
        case (res, el) =>
          // in elisp, first entry wins
          if (res.contains(el._1)) res else res + el
      }
      // props.size counts unique keys. We only create data when keys
      // are not duplicated or we could introduce losses
      if (values.isEmpty || 2 * props.size != values.size)
        None
      else
        Some(props)

    case _ => None
  }
}
