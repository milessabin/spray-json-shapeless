/**
*  Copyright (C) 2010 Aemon Cannon
*
*  This program is free software; you can redistribute it and/or
*  modify it under the terms of the GNU General Public License as
*  published by the Free Software Foundation; either version 2 of
*  the License, or (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public
*  License along with this program; if not, write to the Free
*  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
*  MA 02111-1307, USA.
*/

package org.ensime.server
import java.io.File
import org.ensime.util.FileUtils
import scala.collection.{immutable, mutable}
import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring._
import scala.tools.refactoring.analysis.GlobalIndexes
import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.common.{Change, Selections}
import scala.tools.refactoring.implementations._

case class RefactorFailure(val procedureId: Int, val message: String)
case class RefactorPerformReq(procedureId: Int, refactorType: Symbol, params: immutable.Map[Symbol, Any], interactive: Boolean)
case class RefactorExecReq(procedureId: Int, refactorType: Symbol)
case class RefactorCancelReq(procedureId: Int)

trait RefactorProcedure {
  val procedureId: Int
  val refactorType: scala.Symbol
}
trait RefactorEffect extends RefactorProcedure {
  val changes: Iterable[Change]
}
trait RefactorResult extends RefactorProcedure {
  val touched: Iterable[File]
}

abstract class RefactoringEnvironment(file: String, start: Int, end: Int) {

  val refactoring: MultiStageRefactoring with CompilerAccess

  def performRefactoring(
    procId: Int,
    tpe: scala.Symbol,
    parameters: refactoring.RefactoringParameters): Either[RefactorFailure, RefactorEffect] = {

    val af = AbstractFile.getFile(file)
    val selection = new refactoring.FileSelection(af, refactoring.compilationUnitOfFile(af).get.body, start, end)

    refactoring.prepare(selection) match {
      case Right(prepare) =>
      refactoring.perform(selection, prepare, parameters) match {
        case Right(modifications) => Right(new RefactorEffect {
            val procedureId = procId
            val refactorType = tpe
            val changes = modifications
          })
        case Left(error) => Left(RefactorFailure(procId, error.cause))
      }
      case Left(error) => Left(RefactorFailure(procId, error.cause))
    }
  }
}

trait RefactoringHandler { self: Analyzer =>

  import protocol._

  val effects: mutable.HashMap[Int, RefactorEffect] = new mutable.HashMap

  def handleRefactorRequest(req: RefactorPerformReq, callId: Int) {
    val procedureId = req.procedureId
    val result = scalaCompiler.askPerformRefactor(procedureId, req.refactorType, req.params)

    result match {
      case Right(effect) => {

	if(req.interactive){
          effects(procedureId) = effect
          project ! RPCResultEvent(toWF(effect), callId)
	}
	// Execute the refactoring immediately..
	else{
	  project ! AddUndo("Refactoring of type: " + req.refactorType.toString,
	    FileUtils.inverseChanges(effect.changes))
	  val result = scalaCompiler.askExecRefactor(procedureId, req.refactorType, effect)
	  result match {
	    case Right(result) => {
	      project ! RPCResultEvent(toWF(result), callId)
	    }
	    case Left(f) => project ! RPCResultEvent(toWF(f), callId)
	  }
	}


      }
      case Left(f) => project ! RPCResultEvent(toWF(f), callId)
    }

  }

  def handleRefactorExec(req: RefactorExecReq, callId: Int) {
    val procedureId = req.procedureId
    effects.get(procedureId) match{
      case Some(effect) => {
	project ! AddUndo(
	  "Refactoring of type: " + req.refactorType.toString, 
	  FileUtils.inverseChanges(effect.changes))
	val result = scalaCompiler.askExecRefactor(procedureId, req.refactorType, effect)
	result match {
	  case Right(result) => {
	    project ! RPCResultEvent(toWF(result), callId)
	  }
	  case Left(f) => project ! RPCResultEvent(toWF(f), callId)
	}
      }
      case None => {
	val f = RefactorFailure(procedureId, 
	  "No effect found for procId " + procedureId)
	project ! RPCResultEvent(toWF(f), callId)
      }
    }
  }

  def handleRefactorCancel(req: RefactorCancelReq, callId: Int) {
    effects.remove(req.procedureId)
    project ! RPCResultEvent(toWF(true), callId)
  }

}

trait RefactoringControl { self: RichCompilerControl with RefactoringImpl =>

  def askPerformRefactor(
    procId: Int,
    tpe: scala.Symbol,
    params: immutable.Map[scala.Symbol, Any]): Either[RefactorFailure, RefactorEffect] = {
    askOr(performRefactor(procId, tpe, params), t => Left(RefactorFailure(procId, t.toString)))
  }

  def askExecRefactor(
    procId: Int,
    tpe: scala.Symbol,
    effect: RefactorEffect): Either[RefactorFailure, RefactorResult] = {

    askOr(execRefactor(procId, tpe, effect), t => Left(RefactorFailure(procId, t.toString)))
  }

}

trait RefactoringImpl { self: RichPresentationCompiler =>

  import FileUtils._

  protected def doRename(procId: Int, tpe: scala.Symbol, name: String, file: String, start: Int, end: Int) =
  new RefactoringEnvironment(file, start, end) {
    val refactoring = new Rename with GlobalIndexes {
      val global = RefactoringImpl.this
      val cuIndexes = this.global.unitOfFile.values.map { u => CompilationUnitIndex(u.body) }
      val index = GlobalIndex(cuIndexes.toList)
    }
    val result = performRefactoring(procId, tpe, name)
  }.result

  protected def doExtractMethod(procId: Int, tpe: scala.Symbol, name: String, file: String, start: Int, end: Int) =
  new RefactoringEnvironment(file, start, end) {
    val refactoring = new ExtractMethod with GlobalIndexes {
      val global = RefactoringImpl.this
      val cuIndexes = this.global.unitOfFile.values.map { u => CompilationUnitIndex(u.body) }
      val index = GlobalIndex(cuIndexes.toList)
    }
    val result = performRefactoring(procId, tpe, name)
  }.result

  protected def doExtractLocal(procId: Int, tpe: scala.Symbol, name: String, file: String, start: Int, end: Int) =
  new RefactoringEnvironment(file, start, end) {
    val refactoring = new ExtractLocal with GlobalIndexes {
      val global = RefactoringImpl.this
      val cuIndexes = this.global.unitOfFile.values.map { u => CompilationUnitIndex(u.body) }
      val index = GlobalIndex(cuIndexes.toList)
    }
    val result = performRefactoring(procId, tpe, name)
  }.result

  protected def doInlineLocal(procId: Int, tpe: scala.Symbol, file: String, start: Int, end: Int) =
  new RefactoringEnvironment(file, start, end) {
    val refactoring = new InlineLocal with GlobalIndexes {
      val global = RefactoringImpl.this
      val cuIndexes = this.global.unitOfFile.values.map { u => CompilationUnitIndex(u.body) }
      val index = GlobalIndex(cuIndexes.toList)
    }
    val result = performRefactoring(procId, tpe, new refactoring.RefactoringParameters())
  }.result

  protected def doOrganizeImports(procId: Int, tpe: scala.Symbol, file: String) =
  new RefactoringEnvironment(file, 0, 0) {
    val refactoring = new OrganizeImports {
      val global = RefactoringImpl.this
    }
    val result = performRefactoring(procId, tpe, new refactoring.RefactoringParameters())
  }.result

  protected def doAddImport(procId: Int, tpe: scala.Symbol, qualName: String, file: String, start: Int, end: Int) = {
    val refactoring = new AddImportStatement {
      val global = RefactoringImpl.this
    }
    val af = AbstractFile.getFile(file)
    val selection = new refactoring.FileSelection(af, refactoring.compilationUnitOfFile(af).get.body, start, end)
    val modifications = refactoring.addImport(selection, qualName)
    Right(new RefactorEffect {
        val procedureId = procId
        val refactorType = tpe
        val changes = modifications
      })
  }

  protected def reloadAndType(f: String) = reloadAndTypeFiles(List(getSourceFile(f)))

  protected def performRefactor(
    procId: Int,
    tpe: scala.Symbol,
    params: immutable.Map[scala.Symbol, Any]): Either[RefactorFailure, RefactorEffect] = {

    def badArgs = Left(RefactorFailure(procId, "Incorrect arguments passed to " +
	tpe + ": " + params))

    import org.ensime.util.{Symbols => S}
    try {
      tpe match {
        case S.Rename => {
          (params.get(S.NewName), params.get(S.File), params.get(S.Start), params.get(S.End)) match {
            case (Some(n: String), Some(f: String), Some(s: Int), Some(e: Int)) => {
	      reloadAndType(f)
	      doRename(procId, tpe, n, f, s, e)
            }
            case _ => badArgs
          }
        }
        case S.ExtractMethod => {
          (params.get(S.MethodName), params.get(S.File), params.get(S.Start), params.get(S.End)) match {
            case (Some(n: String), Some(f: String), Some(s: Int), Some(e: Int)) => {
	      reloadAndType(f)
	      doExtractMethod(procId, tpe, n, f, s, e)
            }
            case _ => badArgs
          }
        }
        case S.ExtractLocal => {
          (params.get(S.Name), params.get(S.File), params.get(S.Start), params.get(S.End)) match {
            case (Some(n: String), Some(f: String), Some(s: Int), Some(e: Int)) => {
	      reloadAndType(f)
	      doExtractLocal(procId, tpe, n, f, s, e)
            }
            case _ => badArgs
          }
        }
        case S.InlineLocal => {
          (params.get(S.File), params.get(S.Start), params.get(S.End)) match {
            case (Some(f: String), Some(s: Int), Some(e: Int)) => {
	      reloadAndType(f)
	      doInlineLocal(procId, tpe, f, s, e)
            }
            case _ => badArgs
          }
        }
        case S.OrganizeImports => {
          params.get(S.File) match {
            case Some(f: String) => {
	      reloadAndType(f)
	      doOrganizeImports(procId, tpe, f)
            }
            case _ => badArgs
          }
        }
        case S.AddImport => {
          (params.get(S.QualifiedName), params.get(S.File), params.get(S.Start), params.get(S.End)) match {
            case (Some(n: String), Some(f: String), Some(s: Int), Some(e: Int)) => {
	      reloadAndType(f)
	      doAddImport(procId, tpe, n, f, s, e)
            }
            case _ => badArgs
          }
        }
        case _ => Left(RefactorFailure(procId, "Unknown refactoring: " + tpe))
      }
    } catch {
      case e: Throwable => Left(RefactorFailure(procId, e.toString))
    }
  }

  protected def execRefactor(
    procId: Int,
    refctrType: scala.Symbol,
    effect: RefactorEffect): Either[RefactorFailure, RefactorResult] = {
    writeChanges(effect.changes) match {
      case Right(touchedFiles) => {
        Right(new RefactorResult {
            val refactorType = refctrType
            val procedureId = procId
            val touched = touchedFiles
          })
      }
      case Left(err) => Left(RefactorFailure(procId, err.toString))
    }
  }

}

