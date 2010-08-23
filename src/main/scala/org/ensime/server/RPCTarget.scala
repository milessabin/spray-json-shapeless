package org.ensime.server
import java.io.File
import org.ensime.config.ProjectConfig
import org.ensime.debug.ProjectDebugInfo
import org.ensime.model._
import org.ensime.util._
import scala.actors._
import scala.actors.Actor._
import scala.collection.immutable


trait RPCTarget{ self:Project =>
  
  import protocol._

  def rpcInitProject(conf:ProjectConfig, callId:Int){
    initProject(conf)
    protocol.sendRPCAckOK(callId)
  }

  def rpcReplConfig(callId:Int){
    sendRPCReturn(toWF(this.config.replConfig), callId)
  }

  def rpcDebugConfig(callId:Int) {
    debugInfo = Some(new ProjectDebugInfo(config))
    sendRPCReturn(toWF(this.config.debugConfig), callId)
  }

  def rpcBuilderInit(callId:Int){
    val b = getOrStartBuilder
    b ! RPCRequestEvent(RebuildAllReq(), callId)	
  }

  def rpcBuilderAddFiles(filenames:Iterable[String], callId:Int){
    val files = filenames.map(s => new File(s.toString))
    val b = getOrStartBuilder
    b ! RPCRequestEvent(AddSourceFilesReq(files), callId)
  }

  def rpcBuilderUpdateFiles(filenames:Iterable[String], callId:Int){
    val files = filenames.map(s => new File(s.toString))
    val b = getOrStartBuilder
    b ! RPCRequestEvent(UpdateSourceFilesReq(files), callId)
  }

  def rpcBuilderRemoveFiles(filenames:Iterable[String], callId:Int){
    val files = filenames.map(s => new File(s.toString))
    val b = getOrStartBuilder
    b ! RPCRequestEvent(RemoveSourceFilesReq(files), callId)
  }


  def rpcDebugUnitInfo(sourceName:String, line:Int, packPrefix:String, callId:Int){
    val info = debugInfo.getOrElse(new ProjectDebugInfo(config))
    debugInfo = Some(info)
    val unit = info.findUnit(sourceName, line, packPrefix)
    unit match{
      case Some(unit) => {
	sendRPCReturn(toWF(unit), callId)
      }
      case None => {
	sendRPCReturn(toWF(false), callId)
      }
    }
  }

  def rpcDebugClassLocsToSourceLocs(pairs:Iterable[(String, Int)], callId:Int) {
    val info = debugInfo.getOrElse(new ProjectDebugInfo(config))
    debugInfo = Some(info)
    sendRPCReturn(
      toWF(info.debugClassLocsToSourceLocs(pairs)),
      callId)
  }

  def rpcTypecheckFile(f:String, callId:Int){
    val file:File = new File(f)
    if(f.endsWith(".scala")){
      scalaAnalyzer ! RPCCommandEvent(ReloadFileReq(file))
    }
    else if(f.endsWith("java")){
      javaAnalyzer ! RPCCommandEvent(ReloadFileReq(file))
    }
    sendRPCReturn(toWF(true),callId)
  }

  def rpcTypecheckAll(callId:Int){
    scalaAnalyzer ! RPCCommandEvent(ReloadAllReq())
    javaAnalyzer ! RPCCommandEvent(ReloadAllReq())
    sendRPCReturn(toWF(true),callId)
  }

  def rpcScopeCompletion(f:String, point:Int, prefix:String, constructor:Boolean, callId:Int){
    scalaAnalyzer ! RPCRequestEvent(ScopeCompletionReq(new File(f), point, prefix, constructor), callId)
  }

  def rpcTypeCompletion(f:String, point:Int, prefix:String, callId:Int){
    scalaAnalyzer ! RPCRequestEvent(TypeCompletionReq(new File(f), point, prefix), callId)
  }

  def rpcInspectTypeAtPoint(f:String, point:Int, callId:Int){
    scalaAnalyzer ! RPCRequestEvent(InspectTypeReq(new File(f), point), callId)
  }

  def rpcInspectTypeById(id:Int, callId:Int){
    scalaAnalyzer ! RPCRequestEvent(InspectTypeByIdReq(id), callId)
  }

  def rpcSymbolAtPoint(f:String, point:Int, callId:Int){
    scalaAnalyzer ! RPCRequestEvent(SymbolAtPointReq(new File(f), point), callId)
  }

  def rpcTypeById(id:Int, callId:Int){
    scalaAnalyzer ! RPCRequestEvent(TypeByIdReq(id), callId)
  }

  def rpcCallCompletion(id:Int, callId:Int){
    scalaAnalyzer ! RPCRequestEvent(CallCompletionReq(id), callId)
  }

  def rpcTypeAtPoint(f:String, point:Int, callId:Int){
    scalaAnalyzer ! RPCRequestEvent(TypeAtPointReq(new File(f), point), callId)
  }

  def rpcInspectPackageByPath(path:String, callId:Int){
    scalaAnalyzer ! RPCRequestEvent(InspectPackageByPathReq(path), callId)
  }

  def rpcPerformRefactor(refactorType:Symbol, procId:Int, params:immutable.Map[Symbol, Any], callId:Int){
    scalaAnalyzer ! RPCRequestEvent(RefactorPerformReq(procId, refactorType, params), callId)
  }

  def rpcExecRefactor(refactorType:Symbol, procId:Int, callId:Int){
    scalaAnalyzer ! RPCRequestEvent(RefactorExecReq(procId, refactorType), callId)
  }

  def rpcCancelRefactor(procId:Int, callId:Int){
    scalaAnalyzer ! RPCRequestEvent(RefactorCancelReq(procId), callId)
  }


}
