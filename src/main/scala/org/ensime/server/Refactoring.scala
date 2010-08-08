package org.ensime.server
import java.io.File
import scala.tools.refactoring.implementations._
import scala.tools.refactoring.MultiStageRefactoring
import scala.collection.mutable.{ HashMap }
import scala.tools.refactoring.common.Change


trait RefactorFailure{
  val message:String
}

case class RefactorPrepReq(procedureId:Int, params:Any)
case class RefactorPerformReq(procedureId:Int, params:Any)
case class RefactorExecReq(procedureId:Int, params:Any)

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

class OrganizeImportsPrep(val procedureId:Int) 
extends OrganizeImportsRefactoring with RefactorPrep{
  val prep:impl.PreparationResult
  val selection:impl.FileSelection
}

class OrganizeImportsEffect(val procedureId:Int, val changes:List[Change]) 
extends OrganizeImportsRefactoring with RefactorEffect{}

case class OrganizeImportsParams(file:File)
case class OrganizeImportsPerformParams()
case class OrganizeImportsExecParams()




object RefactoringHelpers{

  implicit def throwableToRefactorFailure(t:Throwable) = {
    new RefactorFailure(){ val message = t.toString }
  }

  implicit def throwableToRefactorFailure(t:MultiStageRefactoring#PreparationError) = {
    new RefactorFailure(){ val message = t.toString }
  }

  implicit def stringToRefactorFailure(s:String) = {
    new RefactorFailure{ val message = s }
  }

}


trait RefactoringController{ self: Compiler =>
  private var counter:Int = 0
  private val inProgress:HashMap[Int, RefactorDesc] = new HashMap

  import protocol._

  val preps:HashMap[Int, RefactorPrep] = new HashMap
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

  val effects:HashMap[Int, RefactorEffect] = new HashMap
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

  import RefactoringHelpers._

  def askPrepRefactor(req:RefactorPrepReq):Either[RefactorFailure, RefactorPrep] = {
    req match{
      case RefactorPrepReq(procId, params:OrganizeImportsPrepParams) => {
	askOr(prepOrganizeImports(procId, params), t => Left(t))
      }
      case _ => throw new IllegalStateException(
	"Attempted to prep an unrecognized refactoring.")
    }
  }

  def askPerformRefactor(req:RefactorPerformReq, prep:RefactorPrep):Either[RefactorFailure, RefactorEffect] = {
    (req,prep) match{
      case (RefactorPerformReq(procId, params:OrganizeImportsPerformParams), prep:OrganizeImportsPrep) => {
	askOr(performOrganizeImports(procId, prep, params), t => Left(t))
      }
      case _ => throw new IllegalStateException(
	"Attempted to perform an unrecognized refactoring.")
    }
  }

  def askExecRefactor(req:RefactorExecReq, effect:RefactorEffect):Either[RefactorFailure, Boolean] = {
    (req,effect) match{
      case (RefactorExecReq(procId, params:OrganizeImportsExecParams), effect:OrganizeImportsEffect) => {
	askOr(execOrganizeImports(procId, effect, params), t => Left(t))
      }
      case _ => throw new IllegalStateException(
	"Attempted to exec an unrecognized refactoring.")
    }
  }

}


trait RefactoringImpl{ self: RichPresentationCompiler =>

  import RefactoringHelpers._


  protected def prepOrganizeImports(
    procId:Int, 
    params:OrganizeImportsPrepParams):Either[RefactorFailure, RefactorPrep] = {

    val source = getSourceFile(params.file.getAbsolutePath)
    val r = new OrganizeImports{
      val global = RefactoringImpl.this
    }
    val sel = r.FileSelection(source.file, 0, source.length - 1)
    r.prepare(sel) match{
      case Right(result) => {
	val prep = new OrganizeImportsPrep(procId){
	  val prep = result
	  val selection = sel
	}
	Right(prep)
      }
      case Left(error) => {
	Left(error)
      }
    }
  }


  protected def performOrganizeImports(
    procId:Int, 
    prep:OrganizeImportsPrep, 
    params:OrganizeImportsPerformParams):Either[RefactorFailure, RefactorEffect] = {

    val result = prep.impl.perform(prep.selection, prep.prep, new prep.impl.RefactoringParameters)
    result match{
      case Right(changes:List[Change]) => Right(
	new OrganizeImportsEffect(prep.procedureId, changes))
      case Left(err) => Left(err.toString)
    }
  }



  protected def execOrganizeImports(
    procId:Int, 
    effect:OrganizeImportsEffect, 
    params:OrganizeImportsExecParams):Either[RefactorFailure, Boolean] = {

    val changes = effect.changes
    Right(true)

  }

}

