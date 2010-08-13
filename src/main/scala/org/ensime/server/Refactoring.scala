package org.ensime.server
import java.io.{IOException, File}
import org.ensime.util._
import scala.collection.{immutable, mutable}
import scala.tools.refactoring._
import scala.tools.refactoring.common.{Selections, Change}
import scala.tools.refactoring.analysis.GlobalIndexes
import scala.tools.refactoring.implementations._


case class RefactorFailure(val procedureId:Int, val message:String)

case class RefactorPrepReq(procedureId:Int, refactorType:Symbol, params:immutable.Map[Symbol, Any])
case class RefactorExecReq(procedureId:Int, refactorType:Symbol)
case class RefactorCancelReq(procedureId:Int)

trait RefactorProcedure{ 
  val procedureId:Int 
  val impl:MultiStageRefactoring
  val refactorType:scala.Symbol
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

  val effects:mutable.HashMap[Int, RefactorEffect] = new mutable.HashMap

  def handleRefactorRequest(req:RefactorPrepReq, callId:Int){
    val procedureId = req.procedureId
    val result = cc.askPrepRefactor(procedureId, req.refactorType, req.params)
    result match{
      case Right(prep) => {
	val result = cc.askPerformRefactor(
	  procedureId, req.refactorType, req.params, prep)
	result match{
	  case Right(effect) => {
	    effects(procedureId) = effect
	    project ! RPCResultEvent(toWF(effect), callId)
	  }
	  case Left(f) => project ! RPCResultEvent(toWF(f), callId)
	}
      }
      case Left(f) => project ! RPCResultEvent(toWF(f), callId)
    }
  }

  def handleRefactorExec(req:RefactorExecReq, callId:Int){
    val procedureId = req.procedureId
    val effect = effects(procedureId)
    val result = cc.askExecRefactor(procedureId, req.refactorType, effect)
    result match{
      case Right(result) => project ! RPCResultEvent(toWF(result), callId)
      case Left(f) => project ! RPCResultEvent(toWF(f), callId)
    }
  }

  def handleRefactorCancel(req:RefactorCancelReq, callId:Int){
    effects.remove(req.procedureId)
    project ! RPCResultEvent(toWF(true), callId)
  }

}



trait RefactoringInterface{ self: RichPresentationCompiler =>

  def askPrepRefactor(
    procId:Int, 
    tpe:scala.Symbol, 
    params:immutable.Map[scala.Symbol, Any]):Either[RefactorFailure, RefactorPrep] = {

    askOr(prepRefactor(procId, tpe, params), 
      t => Left(RefactorFailure(procId, t.toString)))
  }

  def askPerformRefactor(
    procId:Int, 
    tpe:scala.Symbol, 
    params:immutable.Map[scala.Symbol, Any],
    prep:RefactorPrep):Either[RefactorFailure, RefactorEffect] = {

    askOr(performRefactor(procId, tpe, prep, params), 
      t => Left(RefactorFailure(procId, t.toString)))
  }

  def askExecRefactor(
    procId:Int, 
    tpe:scala.Symbol, 
    effect:RefactorEffect):Either[RefactorFailure, RefactorResult] = {

    askOr(execRefactor(procId, tpe, effect), 
      t => Left(RefactorFailure(procId, t.toString)))
  }

}




trait RefactoringImpl{ self: RichPresentationCompiler =>

  import FileUtils._

  protected def prepRefactor(
    procId:Int, 
    refctrType:scala.Symbol,
    params:immutable.Map[scala.Symbol, Any]):Either[RefactorFailure, RefactorPrep] = {

    val r = newRefactoring(refctrType, params)
    val sel = refactoringSelection(refctrType, r, params).asInstanceOf[r.Selection]
    r.prepare(sel) match{
      case Right(result) => {
	val prep = new RefactorPrep{
	  val refactorType = refctrType
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
    refctrType:scala.Symbol,
    prep:RefactorPrep, 
    params:immutable.Map[scala.Symbol, Any]):Either[RefactorFailure, RefactorEffect] = {

    val result = prep.impl.perform(
      prep.selection.asInstanceOf[prep.impl.Selection],
      prep.prep.asInstanceOf[prep.impl.PreparationResult], 
      refactoringParams(refctrType, prep, params).asInstanceOf[prep.impl.RefactoringParameters])

    result match{
      case Right(chngs:List[Change]) => {
	Right(
	  new RefactorEffect{
	    val refactorType = refctrType
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
    refctrType:scala.Symbol,
    effect:RefactorEffect):Either[RefactorFailure, RefactorResult] = {
    writeChanges(effect.changes) match{
      case Right(touchedFiles) => {
	Right(new RefactorResult{
	    val refactorType = refctrType
	    val impl = effect.impl
	    val procedureId = procId
	    val touched = touchedFiles
	  })
      }
      case Left(err) => Left(RefactorFailure(procId, err.toString))
    }
  }


  protected def refactoringParams(
    refctrType:scala.Symbol, 
    prep:RefactorPrep, 
    params:immutable.Map[scala.Symbol, Any]):MultiStageRefactoring#RefactoringParameters = {
    refctrType match{
      case 'organizeImports => {
	val impl = prep.impl.asInstanceOf[OrganizeImports]
	(new impl.RefactoringParameters).asInstanceOf[
	  MultiStageRefactoring#RefactoringParameters]
      }
      case 'rename => {
	val impl = prep.impl.asInstanceOf[Rename]
	(params.get('newName).getOrElse("NO_NAME").toString).asInstanceOf[
	  MultiStageRefactoring#RefactoringParameters]
      }
      case _ => throw new IllegalStateException(
	"Unrecognized refactoring: " + refctrType)
    }
  }

  protected def refactoringSelection(
    refctrType:scala.Symbol, 
    ref:Refactoring, 
    params:immutable.Map[scala.Symbol, Any]):Selections#Selection = {

    val filepath = params.get('file) match{
      case Some(f:String) => f
      case _ => ""
    }
    
    val source = getSourceFile(filepath)

    val point = params.get('point) match{
      case Some(i:Int) => i
      case _ => -1
    }

    refctrType match{
      case 'organizeImports => {
	ref.FileSelection(source.file, 0, source.length - 1)
      }
      case 'rename => {
	val pos = source.position(point)
	ref.TreeSelection(ref.global.typedTreeAt(pos))
      }
      case _ => throw new IllegalStateException(
	"Unrecognized refactoring: " + refctrType)
    }
  }

  protected def newRefactoring(
    refctrType:scala.Symbol, 
    params:immutable.Map[scala.Symbol, Any]):MultiStageRefactoring = {
    refctrType match{
      case 'organizeImports => {
	new OrganizeImports{
	  val global = RefactoringImpl.this
	}
      }
      case 'rename => {
	new Rename with GlobalIndexes{
	  val global = RefactoringImpl.this
	  val cuIndexes = this.global.unitOfFile.values.toList.map{
	    u => CompilationUnitIndex.apply(u.body)
	  }
	  val index = GlobalIndex(cuIndexes) 
	}
      }
      case _ => throw new IllegalStateException(
	"Unrecognized refactoring: " + refctrType)
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

