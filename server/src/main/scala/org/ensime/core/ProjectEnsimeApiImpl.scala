package org.ensime.core

import java.io.File

import akka.actor.ActorRef
import akka.pattern.ask
import org.ensime.EnsimeApi
import org.ensime.model._
import org.ensime.server.ConnectionInfo
import org.ensime.server.protocol._
import org.ensime.util._
import shapeless.Typeable
import shapeless.syntax.typeable._

import scala.concurrent.Await
import scala.concurrent.duration._

trait ProjectEnsimeApiImpl extends EnsimeApi { self: Project =>

  val defaultMaxWait: FiniteDuration = 30.seconds

  def callVoidRPC(target: ActorRef, request: RpcRequest, maxWait: FiniteDuration = defaultMaxWait): Unit = {
    callRPC[VoidResponse.type](target, request, maxWait)
  }

  def callRPC[R](target: ActorRef, request: RpcRequest, maxWait: FiniteDuration = defaultMaxWait)(implicit typ: Typeable[R]): R = {
    val future = target.ask(request)(maxWait)
    val result = Await.result(future, maxWait)
    result match {
      case e: RPCError =>
        throw e
      case r =>
        val castRes = r.cast[R]
        if (castRes.isDefined)
          castRes.get
        else {
          val msg = "Error, incorrect type found in RPC call, expected: " + typ + " got " + r.getClass
          val ex = RPCError(ProtocolConst.ErrExceptionInRPC, msg)
          log.error(msg, ex)
          throw ex
        }
    }
  }

  override def rpcConnectionInfo(): ConnectionInfo = {
    new ConnectionInfo()
  }

  override def rpcShutdownServer(): Unit = {
    shutdownServer()
  }

  override def rpcSubscribeAsync(handler: EnsimeEvent => Unit): Boolean = {
    callRPC[Boolean](actor, SubscribeAsync(handler))
  }

  override def rpcPeekUndo(): Option[Undo] = {
    peekUndo()
  }

  override def rpcExecUndo(undoId: Int): Either[String, UndoResult] = {
    execUndo(undoId)
  }

  override def rpcReplConfig(): ReplConfig = {
    new ReplConfig(config.runtimeClasspath)
  }

  override def rpcSymbolDesignations(file: File, start: Int, end: Int,
    requestedTypes: List[SourceSymbol]): SymbolDesignations = {
    callRPC[SymbolDesignations](getAnalyzer, SymbolDesignationsReq(file, start, end, requestedTypes))
  }

  override def rpcDebugStartVM(commandLine: String): DebugVmStatus = {
    callRPC[DebugVmStatus](acquireDebugger, DebugStartReq(commandLine))
  }

  override def rpcDebugAttachVM(hostname: String, port: String): DebugVmStatus = {
    callRPC[DebugVmStatus](acquireDebugger, DebugAttachReq(hostname, port))
  }

  override def rpcDebugStopVM(): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugStopReq)
  }

  override def rpcDebugRun(): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugRunReq)
  }

  override def rpcDebugContinue(threadId: DebugThreadId): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugContinueReq(threadId))
  }

  override def rpcDebugSetBreakpoint(file: File, line: Int): Unit = {
    callVoidRPC(acquireDebugger, DebugSetBreakReq(file, line))
  }

  override def rpcDebugClearBreakpoint(file: File, line: Int): Unit = {
    callVoidRPC(acquireDebugger, DebugClearBreakReq(file, line))
  }

  override def rpcDebugClearAllBreakpoints(): Unit = {
    callVoidRPC(acquireDebugger, DebugClearAllBreaksReq)
  }

  override def rpcDebugListBreakpoints(): BreakpointList = {
    callRPC[BreakpointList](acquireDebugger, DebugListBreakpointsReq)
  }

  override def rpcDebugNext(threadId: DebugThreadId): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugNextReq(threadId))
  }

  override def rpcDebugStep(threadId: DebugThreadId): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugStepReq(threadId))
  }

  override def rpcDebugStepOut(threadId: DebugThreadId): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugStepOutReq(threadId))
  }

  override def rpcDebugLocateName(threadId: DebugThreadId, name: String): Option[DebugLocation] = {
    callRPC[Option[DebugLocation]](acquireDebugger, DebugLocateNameReq(threadId, name))
  }

  override def rpcDebugValue(loc: DebugLocation): Option[DebugValue] = {
    callRPC[Option[DebugValue]](acquireDebugger, DebugValueReq(loc))
  }

  override def rpcDebugToString(threadId: DebugThreadId, loc: DebugLocation): Option[String] = {
    callRPC[Option[String]](acquireDebugger, DebugToStringReq(threadId, loc))
  }

  override def rpcDebugSetValue(loc: DebugLocation, newValue: String): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugSetValueReq(loc, newValue))
  }

  override def rpcDebugBacktrace(threadId: DebugThreadId, index: Int, count: Int): DebugBacktrace = {
    callRPC[DebugBacktrace](acquireDebugger, DebugBacktraceReq(threadId, index, count))
  }

  override def rpcDebugActiveVM(): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugActiveVmReq)
  }

  override def rpcPatchSource(file: File, edits: List[PatchOp]): Unit = {
    callVoidRPC(getAnalyzer, PatchSourceReq(file, edits))
  }

  override def rpcTypecheckFile(fileInfo: SourceFileInfo): Unit = {
    callVoidRPC(getAnalyzer, TypecheckFileReq(fileInfo))
  }

  override def rpcTypecheckFiles(fs: List[File]): Unit = {
    callVoidRPC(getAnalyzer, TypecheckFilesReq(fs))
  }

  override def rpcRemoveFile(file: File): Unit = {
    callVoidRPC(getAnalyzer, RemoveFileReq(file))
  }

  override def rpcUnloadAll(): Unit = {
    callVoidRPC(getAnalyzer, UnloadAllReq)
  }

  override def rpcTypecheckAll(): Unit = {
    callVoidRPC(getAnalyzer, TypecheckAllReq)
  }

  override def rpcCompletionsAtPoint(fileInfo: SourceFileInfo, point: Int, maxResults: Int,
    caseSens: Boolean, reload: Boolean): CompletionInfoList = {

    callRPC[CompletionInfoList](getAnalyzer, CompletionsReq(fileInfo, point, maxResults, caseSens, reload))
  }

  override def rpcPackageMemberCompletion(path: String, prefix: String): List[CompletionInfo] = {
    callRPC[List[CompletionInfo]](getAnalyzer, PackageMemberCompletionReq(path, prefix))
  }

  override def rpcInspectTypeAtPoint(f: File, range: OffsetRange): Option[TypeInspectInfo] = {
    callRPC[Option[TypeInspectInfo]](getAnalyzer, InspectTypeAtPointReq(f, range))
  }

  override def rpcInspectTypeById(id: Int): Option[TypeInspectInfo] = {
    callRPC[Option[TypeInspectInfo]](getAnalyzer, InspectTypeByIdReq(id))
  }

  override def rpcInspectTypeByName(typeFQN: String): Option[TypeInspectInfo] = {
    callRPC[Option[TypeInspectInfo]](getAnalyzer, InspectTypeByNameReq(typeFQN))
  }

  override def rpcSymbolAtPoint(file: File, point: Int): Option[SymbolInfo] = {
    callRPC[Option[SymbolInfo]](getAnalyzer, SymbolAtPointReq(file, point))
  }

  override def rpcTypeById(typeId: Int): Option[TypeInfo] = {
    callRPC[Option[TypeInfo]](getAnalyzer, TypeByIdReq(typeId))
  }

  override def rpcTypeByName(name: String): Option[TypeInfo] = {
    callRPC[Option[TypeInfo]](getAnalyzer, TypeByNameReq(name))
  }

  override def rpcTypeByNameAtPoint(name: String, file: File, range: OffsetRange): Option[TypeInfo] = {
    callRPC[Option[TypeInfo]](getAnalyzer, TypeByNameAtPointReq(name, file, range))
  }

  override def rpcCallCompletion(typeId: Int): Option[CallCompletionInfo] = {
    callRPC[Option[CallCompletionInfo]](getAnalyzer, CallCompletionReq(typeId))
  }

  override def rpcImportSuggestions(file: File, point: Int, names: List[String], maxResults: Int): ImportSuggestions = {
    callRPC[ImportSuggestions](indexer, ImportSuggestionsReq(file, point, names, maxResults))
  }

  override def rpcDocSignatureAtPoint(file: File, range: OffsetRange): Option[DocSigPair] = {
    callRPC[Option[DocSigPair]](getAnalyzer, DocUriAtPointReq(file, range))
  }

  override def rpcDocSignatureForSymbol(typeFullName: String, memberName: Option[String], signatureString: Option[String]): Option[DocSigPair] = {
    callRPC[Option[DocSigPair]](getAnalyzer, DocUriForSymbolReq(typeFullName, memberName, signatureString))
  }

  override def rpcSymbolByName(typeFullName: String, memberName: Option[String], signatureString: Option[String]): Option[SymbolInfo] = {
    callRPC[Option[SymbolInfo]](getAnalyzer, SymbolByNameReq(typeFullName, memberName, signatureString))
  }

  override def rpcDocUriAtPoint(file: File, range: OffsetRange): Option[String] = {
    rpcDocSignatureAtPoint(file, range).flatMap { sig => callRPC[Option[String]](docServer, DocUriReq(sig)) }
  }

  override def rpcDocUriForSymbol(typeFullName: String, memberName: Option[String], signatureString: Option[String]): Option[String] = {
    rpcDocSignatureForSymbol(typeFullName, memberName, signatureString).flatMap { sig => callRPC[Option[String]](docServer, DocUriReq(sig)) }
  }

  override def rpcPublicSymbolSearch(names: List[String], maxResults: Int): SymbolSearchResults = {
    callRPC[SymbolSearchResults](indexer, PublicSymbolSearchReq(names, maxResults))
  }

  override def rpcUsesOfSymAtPoint(file: File, point: Int): List[ERangePosition] = {
    callRPC[List[ERangePosition]](getAnalyzer, UsesOfSymbolAtPointReq(file, point))
  }

  override def rpcTypeAtPoint(file: File, range: OffsetRange): Option[TypeInfo] = {
    callRPC[Option[TypeInfo]](getAnalyzer, TypeAtPointReq(file, range))
  }

  override def rpcInspectPackageByPath(path: String): Option[PackageInfo] = {
    callRPC[Option[PackageInfo]](getAnalyzer, InspectPackageByPathReq(path))
  }

  override def rpcPrepareRefactor(procId: Int, refactorDesc: RefactorDesc): Either[RefactorFailure, RefactorEffect] = {
    callRPC[Either[RefactorFailure, RefactorEffect]](getAnalyzer, PrepareRefactorReq(procId, null, refactorDesc, interactive = false))
  }

  override def rpcExecRefactor(procId: Int, refactorType: Symbol): Either[RefactorFailure, RefactorResult] = {
    callRPC[Either[RefactorFailure, RefactorResult]](getAnalyzer, ExecRefactorReq(procId, refactorType))
  }

  override def rpcCancelRefactor(procId: Int): Unit = {
    callVoidRPC(getAnalyzer, CancelRefactorReq(procId))
  }

  override def rpcExpandSelection(file: File, start: Int, stop: Int): FileRange = {
    callRPC[FileRange](getAnalyzer, ExpandSelectionReq(file, start, stop))
  }

  override def rpcFormatFiles(files: List[File]): Unit = {
    callVoidRPC(getAnalyzer, FormatSourceReq(files))
  }

  override def rpcFormatFile(fileInfo: SourceFileInfo): String = {
    callRPC[String](getAnalyzer, FormatOneSourceReq(fileInfo))
  }
}
