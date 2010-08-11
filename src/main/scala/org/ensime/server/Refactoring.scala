package org.ensime.server
import java.io.{IOException, File}
import org.ensime.util._
import scala.collection.{immutable, mutable}
import scala.tools.refactoring._
import scala.tools.refactoring.common.{Selections, Change}
import scala.tools.refactoring.implementations._


case class RefactorFailure(val procedureId:Int, val message:String)

case class RefactorPrepReq(procedureId:Int, refactorType:Symbol, params:immutable.Map[Symbol, Any])
case class RefactorPerformReq(procedureId:Int, refactorType:Symbol, params:immutable.Map[Symbol, Any])
case class RefactorExecReq(procedureId:Int, refactorType:Symbol, params:immutable.Map[Symbol, Any])
case class RefactorCancelReq(procedureId:Int)

trait RefactorProcedure{ 
  val procedureId:Int 
  val impl:MultiStageRefactoring
}
trait RefactorPrep extends RefactorProcedure{ 
  val impl:MultiStageRefactoring
  val prep:MultiStageRefactoring#PreparationResult
  val selection:Selections#Selection
}
trait RefactorEffect extends RefactorProcedure{ 
  val changes:Iterable[Change]
}
trait RefactorResult extends RefactorProcedure{ 
  val touched:Iterable[File]
}


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
      case Right(result) => project ! RPCResultEvent(toWF(result), callId)
      case Left(f) => project ! RPCResultEvent(toWF(f), callId)
    }
  }

  def handleRefactorCancel(req:RefactorCancelReq, callId:Int){
    preps.remove(req.procedureId)
    effects.remove(req.procedureId)
    project ! RPCResultEvent(toWF(true), callId)
  }
}



trait RefactoringInterface{ self: RichPresentationCompiler =>

  def askPrepRefactor(req:RefactorPrepReq):Either[RefactorFailure, RefactorPrep] = {
    askOr(prepRefactor(req.procedureId, req.refactorType, req.params), 
      t => Left(RefactorFailure(req.procedureId, t.toString)))
  }

  def askPerformRefactor(req:RefactorPerformReq, prep:RefactorPrep):Either[RefactorFailure, RefactorEffect] = {
    askOr(performRefactor(req.procedureId, req.refactorType, prep, req.params), 
      t => Left(RefactorFailure(req.procedureId, t.toString)))
  }

  def askExecRefactor(req:RefactorExecReq, effect:RefactorEffect):Either[RefactorFailure, RefactorResult] = {
    askOr(execRefactor(req.procedureId, req.refactorType, effect, req.params), 
      t => Left(RefactorFailure(req.procedureId, t.toString)))
  }

}




trait RefactoringImpl{ self: RichPresentationCompiler =>

  import FileUtils._

  protected def prepRefactor(
    procId:Int, 
    refactorType:scala.Symbol,
    params:immutable.Map[scala.Symbol, Any]):Either[RefactorFailure, RefactorPrep] = {

    val filepath = params.get('file).getOrElse(".").toString
    val source = getSourceFile(filepath)
    val r = newRefactoring(refactorType, params)
    val sel = r.FileSelection(source.file, 0, source.length - 1)
    r.prepare(sel) match{
      case Right(result) => {
	val prep = new RefactorPrep{
	  val procedureId = procId
	  val impl = r
	  val prep = result
	  val selection = sel
	}
	Right(prep)
      }
      case Left(err) => {
	Left(RefactorFailure(procId, err.toString))
      }
    }
  }
  protected def performRefactor(
    procId:Int, 
    refactorType:scala.Symbol,
    prep:RefactorPrep, 
    params:immutable.Map[scala.Symbol, Any]):Either[RefactorFailure, RefactorEffect] = {

    val result = prep.impl.perform(
      prep.selection.asInstanceOf[prep.impl.Selection],
      prep.prep.asInstanceOf[prep.impl.PreparationResult], 
      refactoringParams(refactorType, prep, params).asInstanceOf[prep.impl.RefactoringParameters])

    result match{
      case Right(chngs:List[Change]) => {
	Right(
	  new RefactorEffect{
	    val impl = prep.impl
	    val procedureId = procId
	    val changes = chngs
	  })
      }
      case Left(err) => Left(RefactorFailure(procId, err.toString))
    }
  }
  protected def execRefactor(
    procId:Int, 
    refactorType:scala.Symbol,
    effect:RefactorEffect, 
    params:immutable.Map[scala.Symbol, Any]):Either[RefactorFailure, RefactorResult] = {
    writeChanges(effect.changes) match{
      case Right(touchedFiles) => {
	Right(new RefactorResult{
	    val impl = effect.impl
	    val procedureId = procId
	    val touched = touchedFiles
	  })
      }
      case Left(err) => Left(RefactorFailure(procId, err.toString))
    }
  }


  protected def refactoringParams(
    refactorType:scala.Symbol, 
    prep:RefactorPrep, 
    params:immutable.Map[scala.Symbol, Any]):MultiStageRefactoring#RefactoringParameters = {
    refactorType match{
      case 'organizeImports => {
	val impl = prep.impl.asInstanceOf[OrganizeImports]
	(new impl.RefactoringParameters).asInstanceOf[MultiStageRefactoring#RefactoringParameters]
      }
      case _ => throw new IllegalStateException(
	"Unrecognized refactoring: " + refactorType)
    }
  }

  protected def newRefactoring(
    refactorType:scala.Symbol, 
    params:immutable.Map[scala.Symbol, Any]):MultiStageRefactoring = {
    refactorType match{
      case 'organizeImports => {
	new OrganizeImports{
	  val global = RefactoringImpl.this
	}
      }
      case _ => throw new IllegalStateException(
	"Unrecognized refactoring: " + refactorType)
    }
  }


  protected def writeChanges(changes:Iterable[Change]):Either[IOException, Iterable[File]] = {
    val changesByFile = changes.groupBy(_.file)
    val touchedFiles = new mutable.ListBuffer[File]
    try{
      changesByFile.foreach{ pair => 
	val file = pair._1.file
	readFile(file) match{
	  case Right(contents) => 
	  {
	    val changed = Change.applyChanges(pair._2.toList, contents)
	    replaceFileContents(file, changed) match{
	      case Right(s) => {
		touchedFiles += file
	      }
	      case Left(e) => throw e
	    }
	  }
	  case Left(e) => throw e
	}
      }
      Right(touchedFiles.toList)
    }
    catch{
      case e:IOException => Left(e)
    }
  }

}

