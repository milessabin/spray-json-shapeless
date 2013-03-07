/**
 *  Copyright (c) 2010, Aemon Cannon
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *      * Redistributions of source code must retain the above copyright
 *        notice, this list of conditions and the following disclaimer.
 *      * Redistributions in binary form must reproduce the above copyright
 *        notice, this list of conditions and the following disclaimer in the
 *        documentation and/or other materials provided with the distribution.
 *      * Neither the name of ENSIME nor the
 *        names of its contributors may be used to endorse or promote products
 *        derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED. IN NO EVENT SHALL Aemon Cannon BE LIABLE FOR ANY
 *  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 *  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 *  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 *  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.ensime.server

import java.io.File
import org.ensime.util._
import scala.collection.{immutable, mutable}
import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring._
import scala.tools.refactoring.analysis.GlobalIndexes
import scala.tools.refactoring.common.{Change, CompilerAccess, Selections}
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
  val changes: Iterable[FileEdit]
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

    refactoring.compilationUnitOfFile(af) match {
      case Some(cu) => {
        val selection = new refactoring.FileSelection(af, cu.body, start, end)
        refactoring.prepare(selection) match {
          case Right(prepare) =>
            refactoring.perform(selection, prepare, parameters) match {
              case Right(modifications) => Right(new RefactorEffect {
                val procedureId = procId
                val refactorType = tpe
                val changes = modifications.map(FileEdit.fromChange)
              })
              case Left(error) => Left(RefactorFailure(procId, error.cause))
            }
          case Left(error) => Left(RefactorFailure(procId, error.cause))
        }
      }
      case None => {
        Left(RefactorFailure(procId, "Compilation unit not found: " + af))
      }
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
        if (req.interactive) {
          effects(procedureId) = effect
          project ! RPCResultEvent(toWF(effect), callId)
        }
        else { // Execute the refactoring immediately..
          project ! AddUndo("Refactoring of type: " + req.refactorType.toString,
            FileUtils.inverseEdits(effect.changes))
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
    effects.get(procedureId) match {
      case Some(effect) => {
        project ! AddUndo(
          "Refactoring of type: " + req.refactorType.toString,
          FileUtils.inverseEdits(effect.changes))
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

  import org.ensime.util.{ Symbols => S }

  def askPerformRefactor(
    procId: Int,
    tpe: scala.Symbol,
    params: immutable.Map[scala.Symbol, Any]): Either[RefactorFailure, RefactorEffect] = {
    askOption(performRefactor(procId, tpe, params)).getOrElse(
      Left(RefactorFailure(procId, "Refactor call failed")))
  }

  def askExecRefactor(
    procId: Int,
    tpe: scala.Symbol,
    effect: RefactorEffect): Either[RefactorFailure, RefactorResult] = {
    askOption(execRefactor(procId, tpe, effect)).getOrElse(
      Left(RefactorFailure(procId, "Refactor exec call failed."))) match {
      case Right(result) => {
	// Reload all files touched by refactoring, so subsequent refactorings
	// will see consistent state.
	askReloadFiles(result.touched.map(f => createSourceFile(f.getPath)))
	Right(result)
      }
      case Left(failure) => Left(failure)
    }
  }

}

trait RefactoringImpl { self: RichPresentationCompiler =>

  import FileUtils._

  protected def doRename(procId: Int, tpe: scala.Symbol, name: String, file: CanonFile, start: Int, end: Int) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new Rename with GlobalIndexes {
        val global = RefactoringImpl.this
        val invalidSet = toBeRemoved.synchronized { toBeRemoved.toSet }
        val cuIndexes = this.global.activeUnits.map { u => CompilationUnitIndex(u.body) }
        val index = GlobalIndex(cuIndexes.toList)
      }
      val result = performRefactoring(procId, tpe, name)
    }.result

  protected def doExtractMethod(procId: Int, tpe: scala.Symbol,
    name: String, file: CanonFile, start: Int, end: Int) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new ExtractMethod with GlobalIndexes {
        val global = RefactoringImpl.this
        val cuIndexes = this.global.activeUnits.map { u => CompilationUnitIndex(u.body) }
        val index = GlobalIndex(cuIndexes.toList)
      }
      val result = performRefactoring(procId, tpe, name)
    }.result

  protected def doExtractLocal(procId: Int, tpe: scala.Symbol,
    name: String, file: CanonFile, start: Int, end: Int) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new ExtractLocal with GlobalIndexes {
        val global = RefactoringImpl.this
        val cuIndexes = this.global.activeUnits.map { u => CompilationUnitIndex(u.body) }
        val index = GlobalIndex(cuIndexes.toList)
      }
      val result = performRefactoring(procId, tpe, name)
    }.result

  protected def doInlineLocal(procId: Int, tpe: scala.Symbol, file: CanonFile,
    start: Int, end: Int) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new InlineLocal with GlobalIndexes {
        val global = RefactoringImpl.this
        val cuIndexes = this.global.activeUnits.map { u => CompilationUnitIndex(u.body) }
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

  protected def doAddImport(procId: Int, tpe: scala.Symbol, qualName: String,
    file: CanonFile) = {
    val refactoring = new AddImportStatement {
      val global = RefactoringImpl.this
    }
    val af = AbstractFile.getFile(file.getPath)
    val modifications = refactoring.addImport(af, qualName)
    Right(new RefactorEffect {
      val procedureId = procId
      val refactorType = tpe
      val changes = modifications.map(FileEdit.fromChange)
    })
  }

  protected def reloadAndType(f: CanonFile) = reloadAndTypeFiles(List(this.createSourceFile(f.getPath())))

  protected def performRefactor(
    procId: Int,
    tpe: scala.Symbol,
    params: immutable.Map[scala.Symbol, Any]): Either[RefactorFailure, RefactorEffect] = {

    def badArgs = Left(RefactorFailure(procId, "Incorrect arguments passed to " +
      tpe + ": " + params))

    import org.ensime.util.{ Symbols => S }
    try {
      tpe match {
        case S.Rename => {
          (params.get(S.NewName), params.get(S.File), params.get(S.Start),
            params.get(S.End)) match {
              case (Some(n: String), Some(f: String), Some(s: Int), Some(e: Int)) => {
                val file = CanonFile(f)
                reloadAndType(file)
                doRename(procId, tpe, n, file, s, e)
              }
              case _ => badArgs
            }
        }
        case S.ExtractMethod => {
          (params.get(S.MethodName), params.get(S.File), params.get(S.Start),
            params.get(S.End)) match {
              case (Some(n: String), Some(f: String), Some(s: Int), Some(e: Int)) => {
                val file = CanonFile(f)
                reloadAndType(file)
                doExtractMethod(procId, tpe, n, file, s, e)
              }
              case _ => badArgs
            }
        }
        case S.ExtractLocal => {
          (params.get(S.Name), params.get(S.File), params.get(S.Start),
            params.get(S.End)) match {
              case (Some(n: String), Some(f: String), Some(s: Int), Some(e: Int)) => {
                val file = CanonFile(f)
                reloadAndType(file)
                doExtractLocal(procId, tpe, n, file, s, e)
              }
              case _ => badArgs
            }
        }
        case S.InlineLocal => {
          (params.get(S.File), params.get(S.Start), params.get(S.End)) match {
            case (Some(f: String), Some(s: Int), Some(e: Int)) => {
              val file = CanonFile(f)
              reloadAndType(file)
              doInlineLocal(procId, tpe, file, s, e)
            }
            case _ => badArgs
          }
        }
        case S.OrganizeImports => {
          params.get(S.File) match {
            case Some(f: String) => {
              val file = CanonFile(f)
              reloadAndType(file)
              doOrganizeImports(procId, tpe, file)
            }
            case _ => badArgs
          }
        }
        case S.AddImport => {
          (params.get(S.QualifiedName), params.get(S.File)) match {
              case (Some(n: String), Some(f: String)) => {
                val file = CanonFile(f)
                reloadAndType(file)
                doAddImport(procId, tpe, n, file)
              }
              case _ => badArgs
            }
        }
        case _ => Left(RefactorFailure(procId, "Unknown refactoring: " + tpe))
      }
    } catch {
      case e : Throwable => {
	e.printStackTrace()
	Left(RefactorFailure(procId, e.toString))
      }
    }
  }

  protected def execRefactor(
    procId: Int,
    refctrType: scala.Symbol,
    effect: RefactorEffect): Either[RefactorFailure, RefactorResult] = {
    println("Applying changes: " + effect.changes)
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

