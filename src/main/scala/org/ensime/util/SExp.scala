package org.ensime.util

import scala.collection.immutable.Map

sealed trait SExp extends WireFormat {
  override def toWireString: String = toString
  def toScala: Any = toString
}

case class SExpList(items: List[SExp]) extends SExp with Iterable[SExp] {

  override def iterator = items.iterator

  override def toScala: Any = items.map(_.toScala)
  override def toString() = "(" + items.mkString(" ") + ")"

  def toKeywordMap: Map[KeywordAtom, SExp] = {
    var m = Map[KeywordAtom, SExp]()
    items.sliding(2, 2).foreach {
      case (key: KeywordAtom) :: (sexp: SExp) :: rest =>
        m += (key -> sexp)
      case _ =>
    }
    m
  }

  def toSymbolMap: Map[scala.Symbol, Any] = {
    var m = Map[scala.Symbol, Any]()
    items.sliding(2, 2).foreach {
      case SymbolAtom(key) :: (sexp: SExp) :: rest =>
        m += (Symbol(key) -> sexp.toScala)
      case _ =>
    }
    m
  }
}

object BooleanAtom {

  def unapply(z: SExp): Option[Boolean] = z match {
    case TruthAtom() => Some(true)
    case NilAtom() => Some(false)
    case _ => None
  }

}

abstract class BooleanAtom extends SExp {
  def toBool: Boolean
  override def toScala = toBool
}

case class NilAtom() extends BooleanAtom {
  override def toString = "nil"
  override def toBool: Boolean = false

}
case class TruthAtom() extends BooleanAtom {
  override def toString = "t"
  override def toBool: Boolean = true
  override def toScala: Boolean = true
}
case class StringAtom(value: String) extends SExp {
  override def toString = escapeString(value)
  override def toScala = value

  def escapeString(s: String) = {
    val printable = s.flatMap {
      case '\\' => List('\\', '\\')
      case '"' => List('\\', '"')
      case '\n' => List('\\', 'n')
      case '\r' => List('\\', 'r')
      case '\t' => List('\\', 't')
      case x => List(x)
    }
    "\"" + printable + "\""
  }
}
case class IntAtom(value: Int) extends SExp {
  override def toString = String.valueOf(value)
  override def toScala = value
}
case class SymbolAtom(value: String) extends SExp {
  override def toString = value
}
case class KeywordAtom(value: String) extends SExp {
  override def toString = value
}

object SExp {
  def apply(items: SExp*): SExpList = {
    SExpList(items.toList)
  }

  def apply(items: Iterable[SExp]): SExpList = {
    SExpList(items.toList)
  }

  // Helpers for common case of key,val prop-list.
  // Omit keys for nil values.
  def propList(items: (String, SExp)*): SExpList = {
    propList(items)
  }
  def propList(items: Iterable[(String, SExp)]): SExpList = {
    val nonNil = items.filter {
      case (s, NilAtom()) => false
      case (s, SExpList(items)) if items.isEmpty => false
      case _ => true
    }
    SExpList(nonNil.flatMap(ea => List(key(ea._1), ea._2)).toList)
  }

  implicit def strToSExp(str: String): SExp = {
    StringAtom(str)
  }

  def key(str: String): KeywordAtom = {
    KeywordAtom(str)
  }

  implicit def intToSExp(value: Int): SExp = {
    IntAtom(value)
  }

  implicit def boolToSExp(value: Boolean): SExp = {
    if (value) {
      TruthAtom()
    } else {
      NilAtom()
    }
  }

  implicit def symbolToSExp(value: Symbol): SExp = {
    if (value == 'nil) {
      NilAtom()
    } else {
      SymbolAtom(value.toString().drop(1))
    }
  }
}
