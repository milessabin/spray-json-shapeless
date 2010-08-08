package org.ensime.server
import java.io.File
import scala.tools.refactoring.implementations._
import scala.tools.refactoring._
import scala.collection.mutable
import scala.collection.immutable
import scala.tools.refactoring.common.Change


case class RefactorFailure(val procedureId:Int, val message:String)

case class RefactorPrepReq(procedureId:Int, refactorType:Symbol, params:immutable.Map[Symbol, Any])
case class RefactorPerformReq(procedureId:Int, refactorType:Symbol, params:immutable.Map[Symbol, Any])
case class RefactorExecReq(procedureId:Int, refactorType:Symbol, params:immutable.Map[Symbol, Any])

trait RefactorProcedure{ 
  val procedureId:Int 
  val impl:Refactoring
}

trait RefactorPrep extends RefactorProcedure{ 
  val prep:Any
}

trait RefactorEffect extends RefactorProcedure{ 
  val changes:List[Change]
}

trait OrganizeImportsRefactoring extends RefactorProcedure{
  val impl:OrganizeImports
}

abstract class OrganizeImportsPrep(val procedureId:Int)
extends OrganizeImportsRefactoring with RefactorPrep{
  val prep:impl.PreparationResult
  val selection:impl.FileSelection
}

abstract class OrganizeImportsEffect(val procedureId:Int, val changes:List[Change]) 
extends OrganizeImportsRefactoring with RefactorEffect{
}

case class OrganizeImportsParams(file:File)
case class OrganizeImportsPerformParams()
case class OrganizeImportsExecParams()



trait RefactoringController{ self: Compiler =>

  import protocol._

  val preps:mutable.HashMap[Int, RefactorPrep] = new mutable.HashMap
  val effects:mutable.HashMap[Int, RefactorEffect] = new mutable.HashMap


  def handleRefactorRequest(req:RefactorPrepReq, callId:Int){
    val procedureId = req.procedureId
    val result = cc.askPrepRefactor(req)
    result match{
      case Right(prep) => {
	preps(procedureId) = prep
	project ! RPCResultEvent(toWF(prep), callId)
      }
      case Left(f) => project ! RPCResultEvent(toWF(f), callId)
    }
  }

  def handleRefactorPerform(req:RefactorPerformReq, callId:Int){
    val procedureId = req.procedureId
    val prep = preps(procedureId)
    val result = cc.askPerformRefactor(req, prep)
    result match{
      case Right(effect) => {
	effects(procedureId) = effect
	project ! RPCResultEvent(toWF(effect), callId)
      }
      case Left(f) => project ! RPCResultEvent(toWF(f), callId)
    }
  }

  def handleRefactorExec(req:RefactorExecReq, callId:Int){
    val procedureId = req.procedureId
    val effect = effects(procedureId)
    val result = cc.askExecRefactor(req, effect)
    result match{
      case Right(true) => project ! RPCResultEvent(toWF(true), callId)
      case Left(f) => project ! RPCResultEvent(toWF(f), callId)
    }
  }

}

trait RefactoringInterface{ self: RichPresentationCompiler =>


  def askPrepRefactor(req:RefactorPrepReq):Either[RefactorFailure, RefactorPrep] = {
    req.refactorType match{
      case 'organizeImports => {
	askOr(prepOrganizeImports(req.procedureId, req.params), 
	  t => Left(RefactorFailure(req.procedureId, t.toString)))
      }
      case _ => throw new IllegalStateException(
	"Attempted to prep an unrecognized refactoring.")
    }
  }

  def askPerformRefactor(req:RefactorPerformReq, prep:RefactorPrep):Either[RefactorFailure, RefactorEffect] = {
    (req.refactorType, prep) match{
      case ('organizeImports, prep:OrganizeImportsPrep) => {
	askOr(performOrganizeImports(req.procedureId, prep, req.params), 
	  t => Left(RefactorFailure(req.procedureId, t.toString)))
      }
      case _ => throw new IllegalStateException(
	"Attempted to perform an unrecognized refactoring.")
    }
  }

  def askExecRefactor(req:RefactorExecReq, effect:RefactorEffect):Either[RefactorFailure, Boolean] = {
    (req.refactorType, effect) match{
      case ('organizeImports, effect:OrganizeImportsEffect) => {
	askOr(execOrganizeImports(req.procedureId, effect, req.params), 
	  t => Left(RefactorFailure(req.procedureId, t.toString)))
      }
      case _ => throw new IllegalStateException(
	"Attempted to exec an unrecognized refactoring.")
    }
  }

}


trait RefactoringImpl{ self: RichPresentationCompiler =>


  protected def prepOrganizeImports(
    procId:Int, 
    params:immutable.Map[scala.Symbol, Any]):Either[RefactorFailure, RefactorPrep] = {

    val filepath = params.get('file).getOrElse(".").toString
    val source = getSourceFile(filepath)
    val r = new OrganizeImports{
      val global = RefactoringImpl.this
    }
    val sel = r.FileSelection(source.file, 0, source.length - 1)
    r.prepare(sel) match{
      case Right(result) => {
	val prep = new OrganizeImportsPrep(procId){
	  val impl = r
	  val prep = result.asInstanceOf[impl.PreparationResult]
	  val selection = sel.asInstanceOf[impl.FileSelection]
	}
	Right(prep)
      }
      case Left(err) => {
	Left(RefactorFailure(procId, err.toString))
      }
    }
  }


  protected def performOrganizeImports(
    procId:Int, 
    prep:OrganizeImportsPrep, 
    params:immutable.Map[scala.Symbol, Any]):Either[RefactorFailure, RefactorEffect] = {

    val result = prep.impl.perform(prep.selection, prep.prep, new prep.impl.RefactoringParameters)
    result match{
      case Right(changes:List[Change]) => Right(
	new OrganizeImportsEffect(prep.procedureId, changes){
	  val impl = prep.impl
	})
      case Left(err) => Left(RefactorFailure(procId, err.toString))
    }
  }



  protected def execOrganizeImports(
    procId:Int, 
    effect:OrganizeImportsEffect, 
    params:immutable.Map[scala.Symbol, Any]):Either[RefactorFailure, Boolean] = {

    val changes = effect.changes
    println("CHANGES: " + changes)
    Right(true)
  }

}

