package org.ensime.core

import java.io.File

import org.ensime.model._
import org.ensime.util._

import scala.collection.mutable

case class RefactorFailure(
  procedureId: Int,
  reason: String,
  status: scala.Symbol = 'failure // redundant field
  )

trait RefactorProcedure {
  def procedureId: Int
  def refactorType: scala.Symbol
}

case class RefactorEffect(
  procedureId: Int,
  refactorType: scala.Symbol,
  changes: Seq[FileEdit],
  status: scala.Symbol = 'success // redundant field
  ) extends RefactorProcedure

case class RefactorResult(
  procedureId: Int,
  refactorType: scala.Symbol,
  touchedFiles: Seq[File],
  status: scala.Symbol = 'success // redundant field
  ) extends RefactorProcedure

sealed abstract class RefactorDesc(val refactorType: Symbol)

case class InlineLocalRefactorDesc(file: File, start: Int, end: Int) extends RefactorDesc(Symbols.InlineLocal)

case class RenameRefactorDesc(newName: String, file: File, start: Int, end: Int) extends RefactorDesc(Symbols.Rename)

case class ExtractMethodRefactorDesc(methodName: String, file: File, start: Int, end: Int)
  extends RefactorDesc(Symbols.ExtractMethod)

case class ExtractLocalRefactorDesc(name: String, file: File, start: Int, end: Int)
  extends RefactorDesc(Symbols.ExtractLocal)

case class OrganiseImportsRefactorDesc(file: File) extends RefactorDesc(Symbols.OrganizeImports)

case class AddImportRefactorDesc(qualifiedName: String, file: File)
  extends RefactorDesc(Symbols.AddImport)
