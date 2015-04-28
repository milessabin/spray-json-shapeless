package org.ensime.core

import java.io.File

import org.ensime.model._
import org.ensime.server.protocol._
import org.ensime.server.protocol.ProtocolConst._
import org.ensime.util._

import scala.collection.mutable
import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring._
import scala.tools.refactoring.analysis.GlobalIndexes
import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.implementations._
import scalariform.astselect.AstSelector
import scalariform.formatter.ScalaFormatter
import scalariform.parser.ScalaParserException
import scalariform.utils.Range

abstract class RefactoringEnvironment(file: String, start: Int, end: Int) {

  val refactoring: MultiStageRefactoring with CompilerAccess

  def performRefactoring(
    procId: Int,
    tpe: scala.Symbol,
    parameters: refactoring.RefactoringParameters
  ): Either[RefactorFailure, RefactorEffect] = {

    val af = AbstractFile.getFile(file)

    refactoring.compilationUnitOfFile(af) match {
      case Some(cu) =>
        val selection = new refactoring.FileSelection(af, cu.body, start, end)
        refactoring.prepare(selection) match {
          case Right(prepare) =>
            refactoring.perform(selection, prepare, parameters) match {
              case Right(modifications) =>
                val edits = modifications.map(FileEditHelper.fromChange).sorted
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

  def handleRefactorPrepareRequest(req: PrepareRefactorReq): Unit = {
    val procedureId = req.procId
    val refactor = req.params
    val result = scalaCompiler.askPrepareRefactor(procedureId, refactor)

    result match {
      case Right(effect: RefactorEffect) =>
        effects(procedureId) = effect
      case Left(failure) =>
    }

    sender ! result
  }

  def handleRefactorExec(req: ExecRefactorReq): Unit = {
    val procedureId = req.procId
    effects.get(procedureId) match {
      case Some(effect: RefactorEffect) =>
        project ! AddUndo(
          "Refactoring of type: " + req.tpe.toString,
          FileUtils.inverseEdits(effect.changes, charset)
        )
        val result = scalaCompiler.askExecRefactor(procedureId, req.tpe, effect)
        sender ! result
      case None =>
        val f = RefactorFailure(procedureId, "No effect found for procId " + procedureId)
        sender ! Left(f)
    }
  }

  def handleRefactorCancel(req: CancelRefactorReq): Unit = {
    effects.remove(req.procId)
    sender ! VoidResponse
  }

  def handleExecUndo(undo: Undo): Either[String, UndoResult] = {
    FileUtils.writeChanges(undo.changes, charset) match {
      case Right(touched) =>
        handleReloadFiles(touched.toList.map(SourceFileInfo(_)), async = true)
        val sortedTouched = touched.toList.sortBy(_.getCanonicalPath)
        Right(UndoResult(undo.id, sortedTouched))
      case Left(e) => Left(e.getMessage)
    }
  }

  def handleExpandselection(file: File, start: Int, stop: Int): FileRange = {
    try {
      FileUtils.readFile(file, charset) match {
        case Right(contents) =>
          val selectionRange = Range(start, stop - start)
          AstSelector.expandSelection(contents, selectionRange) match {
            case Some(range) => FileRange(file.getPath, range.offset, range.offset + range.length)
            case _ =>
              FileRange(file.getPath, start, stop)
          }
        case Left(e) => throw e
      }
    } catch {
      case e: ScalaParserException =>
        throw RPCError(ErrFormatFailed, "Could not parse broken syntax: " + e)
    }
  }

  def handleFormatFiles(files: List[File]): Unit = {
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
      project ! AddUndo("Formatted source of " + files.mkString(", ") + ".", FileUtils.inverseEdits(changeList, charset))
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

  def handleFormatFile(fileInfo: SourceFileInfo): String = {
    val sourceFile = createSourceFile(fileInfo)
    val contents = sourceFile.content.mkString
    ScalaFormatter.format(contents, config.formattingPrefs)
  }

}

trait RefactoringControl { self: RichCompilerControl with RefactoringImpl =>

  def askPrepareRefactor(
    procId: Int,
    refactor: RefactorDesc
  ): Either[RefactorFailure, RefactorEffect] = {
    askOption(prepareRefactor(procId, refactor)).getOrElse(Left(RefactorFailure(procId, "Refactor call failed")))
  }

  def askExecRefactor(
    procId: Int,
    tpe: scala.Symbol,
    effect: RefactorEffect
  ): Either[RefactorFailure, RefactorResult] = {
    askOption(execRefactor(procId, tpe, effect)).getOrElse(
      Left(RefactorFailure(procId, "Refactor exec call failed."))
    ) match {
        case Right(result) =>
          // Reload all files touched by refactoring, so subsequent refactorings
          // will see consistent state.
          askReloadFiles(result.touchedFiles.map(f => createSourceFile(f.getPath)))
          Right(result)
        case Left(failure) => Left(failure)
      }
  }

}

trait RefactoringImpl { self: RichPresentationCompiler =>

  import org.ensime.util.FileUtils._

  protected def doRename(procId: Int, tpe: scala.Symbol, name: String, file: File, start: Int, end: Int) =
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
    name: String, file: File, start: Int, end: Int) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new ExtractMethod with GlobalIndexes {
        val global = RefactoringImpl.this
        val cuIndexes = this.global.activeUnits().map { u => CompilationUnitIndex(u.body) }
        val index = GlobalIndex(cuIndexes.toList)
      }
      val result = performRefactoring(procId, tpe, name)
    }.result

  protected def doExtractLocal(procId: Int, tpe: scala.Symbol,
    name: String, file: File, start: Int, end: Int) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new ExtractLocal with GlobalIndexes {
        val global = RefactoringImpl.this
        val cuIndexes = this.global.activeUnits().map { u => CompilationUnitIndex(u.body) }
        val index = GlobalIndex(cuIndexes.toList)
      }
      val result = performRefactoring(procId, tpe, name)
    }.result

  protected def doInlineLocal(procId: Int, tpe: scala.Symbol, file: File,
    start: Int, end: Int) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new InlineLocal with GlobalIndexes {
        val global = RefactoringImpl.this
        val cuIndexes = this.global.activeUnits().map { u => CompilationUnitIndex(u.body) }
        val index = GlobalIndex(cuIndexes.toList)
      }
      val result = performRefactoring(procId, tpe, new refactoring.RefactoringParameters())
    }.result

  protected def doOrganizeImports(procId: Int, tpe: scala.Symbol, file: File) =
    new RefactoringEnvironment(file.getPath, 0, 0) {
      val refactoring = new OrganizeImports {
        val global = RefactoringImpl.this
      }
      val result = performRefactoring(procId, tpe, new refactoring.RefactoringParameters())
    }.result

  protected def doAddImport(procId: Int, tpe: scala.Symbol, qualName: String, file: File) = {
    val refactoring = new AddImportStatement {
      val global = RefactoringImpl.this
    }
    val af = AbstractFile.getFile(file.getPath)
    val modifications = refactoring.addImport(af, qualName)
    Right(new RefactorEffect(procId, tpe, modifications.map(FileEditHelper.fromChange)))
  }

  protected def reloadAndType(f: File) = reloadAndTypeFiles(List(this.createSourceFile(f.getPath)))

  protected def prepareRefactor(procId: Int, refactor: RefactorDesc): Either[RefactorFailure, RefactorEffect] = {

    val tpe = refactor.refactorType

    try {
      refactor match {
        case InlineLocalRefactorDesc(file, start, end) =>
          reloadAndType(file)
          doInlineLocal(procId, tpe, file, start, end)
        case RenameRefactorDesc(newName, file, start, end) =>
          reloadAndType(file)
          doRename(procId, tpe, newName, file, start, end)
        case ExtractMethodRefactorDesc(methodName, file, start, end) =>
          reloadAndType(file)
          doExtractMethod(procId, tpe, methodName, file, start, end)
        case ExtractLocalRefactorDesc(name, file, start, end) =>
          reloadAndType(file)
          doExtractLocal(procId, tpe, name, file, start, end)
        case OrganiseImportsRefactorDesc(file) =>
          reloadAndType(file)
          doOrganizeImports(procId, tpe, file)
        case AddImportRefactorDesc(qualifiedName, file) =>
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
    effect: RefactorEffect
  ): Either[RefactorFailure, RefactorResult] = {
    logger.info("Applying changes: " + effect.changes)
    writeChanges(effect.changes, charset) match {
      case Right(touchedFiles) =>
        val sortedTouchedFiles = touchedFiles.toList.sortBy(_.getCanonicalPath)
        Right(new RefactorResult(procId, refactorType, sortedTouchedFiles))
      case Left(err) => Left(RefactorFailure(procId, err.toString))
    }
  }

}

