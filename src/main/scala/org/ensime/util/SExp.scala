package org.ensime.util

import scala.collection.immutable.Map

object SExpExplorer {
  def matchError(msg: String) = new MatchError(msg)
  def apply(exp: SExp): SExpExplorer = {
    exp match {
      case l: SExpList =>
        new SExpListExplorer(l)
      case _ => throw matchError("Cannot convert type " + exp.getClass)
    }
  }
}

import SExpExplorer._

abstract class SExpExplorer(val raw: SExp) {
  def asList: SExpListExplorer = throw matchError("Not a list")
  def asMap: SExpMapExplorer = throw matchError("Not a list/map")
}

class SExpListExplorer(list: SExpList) extends SExpExplorer(list) {
  override def asMap = new SExpMapExplorer(list)
  override def asList = this
  def get(index: Int): SExpExplorer = SExpExplorer(list.iterator.toList(index))
}

class SExpMapExplorer(list: SExpList) extends SExpListExplorer(list) {
  val map = list.toKeywordMap

  def getString(name: String): String = getStringOpt(name).getOrElse(throw matchError("required key " + name + " not found"))

  def getStringOpt(name: String): Option[String] = map.get(KeywordAtom(name)) match {
    case Some(StringAtom(s)) => Some(s)
    case Some(_) => throw matchError("Key " + name + " does not refer to a string element")
    case None => None
  }

  def getBoolean(name: String): Boolean = getBooleanOpt(name).getOrElse(throw matchError("required key " + name + " not found"))

  def getBooleanOpt(name: String): Option[Boolean] = map.get(KeywordAtom(name)) match {
    case Some(BooleanAtom(s)) => Some(s)
    case Some(_) => throw matchError("Key " + name + " does not refer to a string element")
    case None => None
  }

  def getStringListOpt(name: String): Option[List[String]] = getStringListOpt(KeywordAtom(name))
  def getStringListOpt(name: KeywordAtom): Option[List[String]] = {
    map.get(name) match {
      case Some(SExpList(items: Iterable[_])) => Some(items.map {
        case s: StringAtom => s.value
        case _ =>
          throw matchError("Expecting a list of string values at key: " + name)
      }.toList)
      case Some(NilAtom) => Some(List.empty)
      case _ =>
        None
    }
  }

  def getStringList(name: String): List[String] = getStringList(KeywordAtom(name))
  def getStringList(name: KeywordAtom): List[String] = getStringListOpt(name).getOrElse(
    throw matchError("Expecting a list of string values at key: " + name))

  def getList(name: String): List[SExpExplorer] = getList(KeywordAtom(name))
  def getList(name: KeywordAtom): List[SExpExplorer] = {
    map.get(name) match {
      case Some(SExpList(items)) =>
        items.map(SExpExplorer(_)).toList
      case _ =>
        throw matchError("Expecting a list of string values at key: " + name)
    }
  }
}

sealed trait SExp extends WireFormat {
  override def toWireString: String = toString
  def toScala: Any = toString
}

object SExpList {
  def apply(): SExpList = SExpList(Nil)
}
trait Atom extends SExp

case class SExpList(items: List[SExp]) extends SExp with Iterable[SExp] {

  override def iterator = items.iterator

  override def toScala: Any = items.map(_.toScala)
  override def toString() = "(" + items.mkString(" ") + ")"

  /**
   * @return If list contents are all Strings, items converted to a List[String] else None
   */
  def toStringList: Option[List[String]] = {
    Some(items.map {
      case StringAtom(value) => value
      case _ => return None
    })
  }

  def toKeywordMap: Map[KeywordAtom, SExp] = {
    var m = Map[KeywordAtom, SExp]()
    items.sliding(2, 2).foreach {
      case (key: KeywordAtom) :: (sexp: SExp) :: rest =>
        m += (key -> sexp)
      case _ =>
        throw new IllegalArgumentException("SExp is not of correct form (keyword value)* to convert to map")
    }
    m
  }

  def toSymbolMap: Option[Map[scala.Symbol, Any]] = {
    Some(items.sliding(2, 2).map {
      case SymbolAtom(key) :: (sexp: Atom) :: Nil =>
        Symbol(key) -> sexp.toScala
      case _ =>
        return None
    }.toMap)
  }
}

object BooleanAtom {

  def unapply(z: SExp): Option[Boolean] = z match {
    case TruthAtom => Some(true)
    case NilAtom => Some(false)
    case _ => None
  }

}

abstract class BooleanAtom extends Atom {
  def toBool: Boolean
  override def toScala = toBool
}

case object NilAtom extends BooleanAtom {
  override def toString = "nil"
  override def toBool: Boolean = false

}
case object TruthAtom extends BooleanAtom {
  override def toString = "t"
  override def toBool: Boolean = true
  override def toScala: Boolean = true
}
case class StringAtom(value: String) extends Atom {
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
case class IntAtom(value: Int) extends Atom {
  override def toString = String.valueOf(value)
  override def toScala = value
}
case class SymbolAtom(value: String) extends Atom {
  override def toString = value
}
case class KeywordAtom(value: String) extends Atom {
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
      case (s, NilAtom) => false
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
      TruthAtom
    } else {
      NilAtom
    }
  }

  implicit def symbolToSExp(value: Symbol): SExp = {
    if (value == 'nil) {
      NilAtom
    } else {
      SymbolAtom(value.toString().drop(1))
    }
  }
}
