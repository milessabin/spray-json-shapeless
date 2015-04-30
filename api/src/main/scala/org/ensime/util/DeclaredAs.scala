package org.ensime.util

sealed abstract class DeclaredAs(val symbol: scala.Symbol)

object DeclaredAs {
  case object Method extends DeclaredAs('method)
  case object Trait extends DeclaredAs('trait)
  case object Interface extends DeclaredAs('interface)
  case object Object extends DeclaredAs('object)
  case object Class extends DeclaredAs('class)
  case object Field extends DeclaredAs('field)
  case object Nil extends DeclaredAs('nil)

  def allDeclarations = Seq(Method, Trait, Interface, Object, Class, Field, Nil)
}
