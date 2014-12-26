package org.ensime.server

import java.io.File

import org.ensime.model.AddUndo
import org.ensime.model.SourceFileInfo
import org.ensime.model.Undo
import org.ensime.model.UndoResult
import org.ensime.protocol.ProtocolConst._
import org.ensime.util._

import scala.collection.mutable
import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring._
import scala.tools.refactoring.analysis.GlobalIndexes
import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.implementations._
import scalariform.formatter.ScalaFormatter
import scalariform.parser.ScalaParserException
import scalariform.astselect.AstSelector
import scalariform.utils.Range

case class RefactorFailure(procedureId: Int, message: String)
case class RefactorPrepareReq(procedureId: Int, refactor: RefactorDesc) extends RPCRequest
case class RefactorExecReq(procedureId: Int, refactorType: Symbol) extends RPCRequest
case class RefactorCancelReq(procedureId: Int) extends RPCRequest

trait RefactorProcedure {
  val procedureId: Int
  val refactorType: scala.Symbol
}

case class RefactorEffect(procedureId: Int,
  refactorType: scala.Symbol,
  changes: Seq[FileEdit]) extends RefactorProcedure

case class RefactorResult(procedureId: Int,
  refactorType: scala.Symbol,
  touched: Seq[File]) extends RefactorProcedure

sealed abstract class RefactorDesc(val refactorType: Symbol)

case class InlineLocalRefactorDesc(file: String, start: Int, end: Int) extends RefactorDesc(Symbols.InlineLocal)

case class RenameRefactorDesc(newName: String, file: String, start: Int, end: Int) extends RefactorDesc(Symbols.Rename)

case class ExtractMethodRefactorDesc(methodName: String, file: String, start: Int, end: Int)
  extends RefactorDesc(Symbols.ExtractMethod)

case class ExtractLocalRefactorDesc(name: String, file: String, start: Int, end: Int)
  extends RefactorDesc(Symbols.ExtractLocal)

case class OrganiseImportsRefactorDesc(file: String) extends RefactorDesc(Symbols.OrganizeImports)

case class AddImportRefactorDesc(qualifiedName: String, file: String)
  extends RefactorDesc(Symbols.AddImport)

abstract class RefactoringEnvironment(file: String, start: Int, end: Int) {

  val refactoring: MultiStageRefactoring with CompilerAccess

  def performRefactoring(
    procId: Int,
    tpe: scala.Symbol,
    parameters: refactoring.RefactoringParameters): Either[RefactorFailure, RefactorEffect] = {

    val af = AbstractFile.getFile(file)

    refactoring.compilationUnitOfFile(af) match {
      case Some(cu) =>
        val selection = new refactoring.FileSelection(af, cu.body, start, end)
        refactoring.prepare(selection) match {
          case Right(prepare) =>
            refactoring.perform(selection, prepare, parameters) match {
              case Right(modifications) =>
                val edits = modifications.map(FileEdit.fromChange).sorted
                Right(new RefactorEffect(procId, tpe, edits))
              case Left(error) => Left(RefactorFailure(procId, error.cause))
            }
          case Left(error) => Left(RefactorFailure(procId, error.cause))
        }
      case None =>
        Left(RefactorFailure(procId, "Compilation unit not found: " + af))
    }

  }
}

trait RefactoringHandler { self: Analyzer =>

  val effects: mutable.HashMap[Int, RefactorEffect] = new mutable.HashMap

  def handleRefactorPrepareRequest(req: RefactorPrepareReq) {
    val procedureId = req.procedureId
    val refactor = req.refactor
    val result = scalaCompiler.askPrepareRefactor(procedureId, refactor)

    result match {
      case Right(effect: RefactorEffect) =>
        effects(procedureId) = effect
      case Left(failure) =>
    }

    sender ! result
  }

  def handleRefactorExec(req: RefactorExecReq) {
    val procedureId = req.procedureId
    effects.get(procedureId) match {
      case Some(effect: RefactorEffect) =>
        project ! AddUndo(
          "Refactoring of type: " + req.refactorType.toString,
          FileUtils.inverseEdits(effect.changes, charset))
        val result = scalaCompiler.askExecRefactor(procedureId, req.refactorType, effect)
        sender ! result
      case None =>
        val f = RefactorFailure(procedureId, "No effect found for procId " + procedureId)
        sender ! Left(f)
    }
  }

  def handleRefactorCancel(req: RefactorCancelReq) {
    effects.remove(req.procedureId)
    sender ! VoidResponse
  }

  def handleExecUndo(undo: Undo): Either[String, UndoResult] = {
    FileUtils.writeChanges(undo.changes, charset) match {
      case Right(touched) =>
        handleReloadFiles(touched.toList.map { SourceFileInfo(_) })
        val sortedTouched = touched.toList.sortBy(_.getCanonicalPath)
        Right(UndoResult(undo.id, sortedTouched))
      case Left(e) => Left(e.getMessage)
    }
  }

  def handleExpandselection(filename: String, start: Int, stop: Int): FileRange = {
    try {
      FileUtils.readFile(new File(filename), charset) match {
        case Right(contents) =>
          val selectionRange = Range(start, stop - start)
          AstSelector.expandSelection(contents, selectionRange) match {
            case Some(range) => FileRange(filename, range.offset, range.offset + range.length)
            case _ =>
              FileRange(filename, start, stop)
          }
        case Left(e) => throw e
      }
    } catch {
      case e: ScalaParserException =>
        throw RPCError(ErrFormatFailed, "Could not parse broken syntax: " + e)
    }
  }

  def handleFormatFiles(filenames: List[String]): Unit = {
    val files = filenames.map { new File(_) }
    try {
      val cs = charset
      val changeList = files.map { f =>
        FileUtils.readFile(f, cs) match {
          case Right(contents) =>
            val formatted = ScalaFormatter.format(contents, config.formattingPrefs)
            TextEdit(f, 0, contents.length, formatted)
          case Left(e) => throw e
        }
      }
      project ! AddUndo("Formatted source of " + filenames.mkString(", ") + ".", FileUtils.inverseEdits(changeList, charset))
      FileUtils.writeChanges(changeList, cs) match {
        case Right(_) =>
        // do nothing - returning signals success
        case Left(e) =>
          throw RPCError(ErrFormatFailed, "Could not write any formatting changes: " + e)
      }
    } catch {
      case e: ScalaParserException =>
        throw RPCError(ErrFormatFailed, "Cannot format broken syntax: " + e)
    }
  }
}

trait RefactoringControl { self: RichCompilerControl with RefactoringImpl =>

  def askPrepareRefactor(
    procId: Int,
    refactor: RefactorDesc): Either[RefactorFailure, RefactorEffect] = {
    askOption(prepareRefactor(procId, refactor)).getOrElse(Left(RefactorFailure(procId, "Refactor call failed")))
  }

  def askExecRefactor(
    procId: Int,
    tpe: scala.Symbol,
    effect: RefactorEffect): Either[RefactorFailure, RefactorResult] = {
    askOption(execRefactor(procId, tpe, effect)).getOrElse(
      Left(RefactorFailure(procId, "Refactor exec call failed."))) match {
        case Right(result) =>
          // Reload all files touched by refactoring, so subsequent refactorings
          // will see consistent state.
          askReloadFiles(result.touched.map(f => createSourceFile(f.getPath)))
          Right(result)
        case Left(failure) => Left(failure)
      }
  }

}

trait RefactoringImpl { self: RichPresentationCompiler =>

  import org.ensime.util.FileUtils._

  protected def doRename(procId: Int, tpe: scala.Symbol, name: String, file: CanonFile, start: Int, end: Int) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new Rename with GlobalIndexes {
        val global = RefactoringImpl.this
        val invalidSet = toBeRemoved.synchronized { toBeRemoved.toSet }
        val cuIndexes = this.global.activeUnits().map { u => CompilationUnitIndex(u.body) }
        val index = GlobalIndex(cuIndexes.toList)
      }
      val result = performRefactoring(procId, tpe, name)
    }.result

  protected def doExtractMethod(procId: Int, tpe: scala.Symbol,
    name: String, file: CanonFile, start: Int, end: Int) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new ExtractMethod with GlobalIndexes {
        val global = RefactoringImpl.this
        val cuIndexes = this.global.activeUnits().map { u => CompilationUnitIndex(u.body) }
        val index = GlobalIndex(cuIndexes.toList)
      }
      val result = performRefactoring(procId, tpe, name)
    }.result

  protected def doExtractLocal(procId: Int, tpe: scala.Symbol,
    name: String, file: CanonFile, start: Int, end: Int) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new ExtractLocal with GlobalIndexes {
        val global = RefactoringImpl.this
        val cuIndexes = this.global.activeUnits().map { u => CompilationUnitIndex(u.body) }
        val index = GlobalIndex(cuIndexes.toList)
      }
      val result = performRefactoring(procId, tpe, name)
    }.result

  protected def doInlineLocal(procId: Int, tpe: scala.Symbol, file: CanonFile,
    start: Int, end: Int) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new InlineLocal with GlobalIndexes {
        val global = RefactoringImpl.this
        val cuIndexes = this.global.activeUnits().map { u => CompilationUnitIndex(u.body) }
        val index = GlobalIndex(cuIndexes.toList)
      }
      val result = performRefactoring(procId, tpe, new refactoring.RefactoringParameters())
    }.result

  protected def doOrganizeImports(procId: Int, tpe: scala.Symbol, file: CanonFile) =
    new RefactoringEnvironment(file.getPath, 0, 0) {
      val refactoring = new OrganizeImports {
        val global = RefactoringImpl.this
      }
      val result = performRefactoring(procId, tpe, new refactoring.RefactoringParameters())
    }.result

  protected def doAddImport(procId: Int, tpe: scala.Symbol, qualName: String, file: CanonFile) = {
    val refactoring = new AddImportStatement {
      val global = RefactoringImpl.this
    }
    val af = AbstractFile.getFile(file.getPath)
    val modifications = refactoring.addImport(af, qualName)
    Right(new RefactorEffect(procId, tpe, modifications.map(FileEdit.fromChange)))
  }

  protected def reloadAndType(f: CanonFile) = reloadAndTypeFiles(List(this.createSourceFile(f.getPath)))

  protected def prepareRefactor(procId: Int, refactor: RefactorDesc): Either[RefactorFailure, RefactorEffect] = {

    val tpe = refactor.refactorType

    try {
      refactor match {
        case InlineLocalRefactorDesc(filename, start, end) =>
          val file = CanonFile(filename)
          reloadAndType(file)
          doInlineLocal(procId, tpe, file, start, end)
        case RenameRefactorDesc(newName, filename, start, end) =>
          val file = CanonFile(filename)
          reloadAndType(file)
          doRename(procId, tpe, newName, file, start, end)
        case ExtractMethodRefactorDesc(methodName, filename, start, end) =>
          val file = CanonFile(filename)
          reloadAndType(file)
          doExtractMethod(procId, tpe, methodName, file, start, end)
        case ExtractLocalRefactorDesc(name, filename, start, end) =>
          val file = CanonFile(filename)
          reloadAndType(file)
          doExtractLocal(procId, tpe, name, file, start, end)
        case OrganiseImportsRefactorDesc(filename: String) =>
          val file = CanonFile(filename)
          reloadAndType(file)
          doOrganizeImports(procId, tpe, file)
        case AddImportRefactorDesc(qualifiedName, filename) =>
          val file = CanonFile(filename)
          reloadAndType(file)
          doAddImport(procId, tpe, qualifiedName, file)
      }
    } catch {
      case e: Throwable =>
        logger.error("Error during refactor request: " + refactor, e)
        Left(RefactorFailure(procId, e.toString))
    }
  }

  protected def execRefactor(
    procId: Int,
    refactorType: scala.Symbol,
    effect: RefactorEffect): Either[RefactorFailure, RefactorResult] = {
    logger.info("Applying changes: " + effect.changes)
    writeChanges(effect.changes, charset) match {
      case Right(touchedFiles) =>
        val sortedTouchedFiles = touchedFiles.toList.sortBy(_.getCanonicalPath)
        Right(new RefactorResult(procId, refactorType, sortedTouchedFiles))
      case Left(err) => Left(RefactorFailure(procId, err.toString))
    }
  }

}

