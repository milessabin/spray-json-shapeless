package org.ensime.util

sealed abstract class RefactorType(val symbol: Symbol)

object RefactorType {
  case object Rename extends RefactorType('rename)
  case object ExtractMethod extends RefactorType('extractMethod)
  case object ExtractLocal extends RefactorType('extractLocal)
  case object InlineLocal extends RefactorType('inlineLocal)
  case object OrganizeImports extends RefactorType('organizeImports)
  case object AddImport extends RefactorType('addImport)

  def allTypes = Seq(Rename, ExtractMethod, ExtractLocal, InlineLocal, OrganizeImports, AddImport)
}
