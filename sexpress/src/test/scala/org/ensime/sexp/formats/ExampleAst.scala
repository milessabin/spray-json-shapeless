package org.ensime.sexp.formats

/**
 * An example Abstract Syntax Tree / family.
 */
object ExampleAst {
  sealed trait Token {
    def text: String
  }

  sealed trait RawToken extends Token
  case class Split(text: String) extends RawToken
  case class And(text: String) extends RawToken
  case class Or(text: String) extends RawToken

  sealed trait ContextualMarker extends RawToken
  case class Like(text: String) extends ContextualMarker
  case class Prefer(text: String) extends ContextualMarker
  case class Negate(text: String) extends ContextualMarker

  sealed trait TokenTree extends Token
  sealed trait ContextualToken extends TokenTree
  sealed trait CompressedToken extends TokenTree
  case class Unparsed(text: String) extends TokenTree
  case class AndCondition(left: TokenTree, right: TokenTree, text: String) extends TokenTree
  case class OrCondition(left: TokenTree, right: TokenTree, text: String) extends TokenTree

  case class Ignored(text: String = "") extends TokenTree
  case class Unclear(text: String = "") extends TokenTree

  object SpecialToken extends TokenTree {
    // to test case object serialisation
    def text = ""
  }

  sealed trait Term extends TokenTree {
    def field: DatabaseField
  }

  case class DatabaseField(column: String)
  case class FieldTerm(text: String, field: DatabaseField, value: String) extends Term
  case class BoundedTerm(
    text: String,
    field: DatabaseField,
    low: Option[String] = None,
    high: Option[String] = None,
    inclusive: Boolean = false
  ) extends Term
  case class LikeTerm(term: FieldTerm, like: Option[Like]) extends Term {
    val text = like.map(_.text).getOrElse("")
    val field = term.field
  }
  case class PreferToken(tree: TokenTree, before: Option[Prefer], after: Option[Prefer]) extends TokenTree {
    val text = before.getOrElse("") + tree.text + after.getOrElse("")
  }
  case class InTerm(field: DatabaseField, value: List[String], text: String = "") extends CompressedToken

  case class QualifierToken(text: String, field: DatabaseField) extends ContextualToken with Term
}
