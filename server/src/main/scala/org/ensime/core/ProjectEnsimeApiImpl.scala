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

  def callRPC[R](target: ActorRef, request: Any, maxWait: FiniteDuration = defaultMaxWait)(implicit typ: Typeable[R]): R = {
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

  override def connectionInfo(): ConnectionInfo = {
    new ConnectionInfo()
  }

  override def subscribeAsync(handler: EnsimeEvent => Unit): Boolean = {
    callRPC[Boolean](actor, SubscribeAsync(handler))
  }

  override def replConfig(): ReplConfig = {
    new ReplConfig(config.runtimeClasspath)
  }

  override def symbolDesignations(file: File, start: Int, end: Int,
    requestedTypes: List[SourceSymbol]): SymbolDesignations = {
    callRPC[SymbolDesignations](getAnalyzer, SymbolDesignationsReq(file, start, end, requestedTypes))
  }

  override def debugStartVM(commandLine: String): DebugVmStatus = {
    callRPC[DebugVmStatus](acquireDebugger, DebugStartReq(commandLine))
  }

  override def debugAttachVM(hostname: String, port: String): DebugVmStatus = {
    callRPC[DebugVmStatus](acquireDebugger, DebugAttachReq(hostname, port))
  }

  override def debugStopVM(): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugStopReq)
  }

  override def debugRun(): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugRunReq)
  }

  override def debugContinue(threadId: DebugThreadId): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugContinueReq(threadId))
  }

  override def debugSetBreakpoint(file: File, line: Int): Unit = {
    callVoidRPC(acquireDebugger, DebugSetBreakReq(file, line))
  }

  override def debugClearBreakpoint(file: File, line: Int): Unit = {
    callVoidRPC(acquireDebugger, DebugClearBreakReq(file, line))
  }

  override def debugClearAllBreakpoints(): Unit = {
    callVoidRPC(acquireDebugger, DebugClearAllBreaksReq)
  }

  override def debugListBreakpoints(): BreakpointList = {
    callRPC[BreakpointList](acquireDebugger, DebugListBreakpointsReq)
  }

  override def debugNext(threadId: DebugThreadId): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugNextReq(threadId))
  }

  override def debugStep(threadId: DebugThreadId): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugStepReq(threadId))
  }

  override def debugStepOut(threadId: DebugThreadId): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugStepOutReq(threadId))
  }

  override def debugLocateName(threadId: DebugThreadId, name: String): Option[DebugLocation] = {
    callRPC[Option[DebugLocation]](acquireDebugger, DebugLocateNameReq(threadId, name))
  }

  override def debugValue(loc: DebugLocation): Option[DebugValue] = {
    callRPC[Option[DebugValue]](acquireDebugger, DebugValueReq(loc))
  }

  override def debugToString(threadId: DebugThreadId, loc: DebugLocation): Option[String] = {
    callRPC[Option[String]](acquireDebugger, DebugToStringReq(threadId, loc))
  }

  override def debugSetValue(loc: DebugLocation, newValue: String): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugSetValueReq(loc, newValue))
  }

  override def debugBacktrace(threadId: DebugThreadId, index: Int, count: Int): DebugBacktrace = {
    callRPC[DebugBacktrace](acquireDebugger, DebugBacktraceReq(threadId, index, count))
  }

  override def debugActiveVM(): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugActiveVmReq)
  }

  override def patchSource(file: File, edits: List[PatchOp]): Unit = {
    callVoidRPC(getAnalyzer, PatchSourceReq(file, edits))
  }

  override def typecheckFile(fileInfo: SourceFileInfo): Unit = {
    callVoidRPC(getAnalyzer, TypecheckFileReq(fileInfo))
  }

  override def typecheckFiles(fs: List[File]): Unit = {
    callVoidRPC(getAnalyzer, TypecheckFilesReq(fs))
  }

  override def removeFile(file: File): Unit = {
    callVoidRPC(getAnalyzer, RemoveFileReq(file))
  }

  override def unloadAll(): Unit = {
    callVoidRPC(getAnalyzer, UnloadAllReq)
  }

  override def typecheckAll(): Unit = {
    callVoidRPC(getAnalyzer, TypecheckAllReq)
  }

  override def completionsAtPoint(fileInfo: SourceFileInfo, point: Int, maxResults: Int,
    caseSens: Boolean, reload: Boolean): CompletionInfoList = {

    callRPC[CompletionInfoList](getAnalyzer, CompletionsReq(fileInfo, point, maxResults, caseSens, reload))
  }

  override def packageMemberCompletion(path: String, prefix: String): List[CompletionInfo] = {
    callRPC[List[CompletionInfo]](getAnalyzer, PackageMemberCompletionReq(path, prefix))
  }

  override def inspectTypeAtPoint(f: File, range: OffsetRange): Option[TypeInspectInfo] = {
    callRPC[Option[TypeInspectInfo]](getAnalyzer, InspectTypeAtPointReq(f, range))
  }

  override def inspectTypeById(id: Int): Option[TypeInspectInfo] = {
    callRPC[Option[TypeInspectInfo]](getAnalyzer, InspectTypeByIdReq(id))
  }

  override def inspectTypeByName(typeFQN: String): Option[TypeInspectInfo] = {
    callRPC[Option[TypeInspectInfo]](getAnalyzer, InspectTypeByNameReq(typeFQN))
  }

  override def symbolAtPoint(file: File, point: Int): Option[SymbolInfo] = {
    callRPC[Option[SymbolInfo]](getAnalyzer, SymbolAtPointReq(file, point))
  }

  override def typeById(typeId: Int): Option[TypeInfo] = {
    callRPC[Option[TypeInfo]](getAnalyzer, TypeByIdReq(typeId))
  }

  override def typeByName(name: String): Option[TypeInfo] = {
    callRPC[Option[TypeInfo]](getAnalyzer, TypeByNameReq(name))
  }

  override def typeByNameAtPoint(name: String, file: File, range: OffsetRange): Option[TypeInfo] = {
    callRPC[Option[TypeInfo]](getAnalyzer, TypeByNameAtPointReq(name, file, range))
  }

  override def callCompletion(typeId: Int): Option[CallCompletionInfo] = {
    callRPC[Option[CallCompletionInfo]](getAnalyzer, CallCompletionReq(typeId))
  }

  override def importSuggestions(file: File, point: Int, names: List[String], maxResults: Int): ImportSuggestions = {
    callRPC[ImportSuggestions](indexer, ImportSuggestionsReq(file, point, names, maxResults))
  }

  override def docSignatureAtPoint(file: File, range: OffsetRange): Option[DocSigPair] = {
    callRPC[Option[DocSigPair]](getAnalyzer, DocUriAtPointReq(file, range))
  }

  override def docSignatureForSymbol(typeFullName: String, memberName: Option[String], signatureString: Option[String]): Option[DocSigPair] = {
    callRPC[Option[DocSigPair]](getAnalyzer, DocUriForSymbolReq(typeFullName, memberName, signatureString))
  }

  override def symbolByName(typeFullName: String, memberName: Option[String], signatureString: Option[String]): Option[SymbolInfo] = {
    callRPC[Option[SymbolInfo]](getAnalyzer, SymbolByNameReq(typeFullName, memberName, signatureString))
  }

  override def docUriAtPoint(file: File, range: OffsetRange): Option[String] = {
    docSignatureAtPoint(file, range).flatMap { sig => callRPC[Option[String]](docServer, DocUriReq(sig)) }
  }

  override def docUriForSymbol(typeFullName: String, memberName: Option[String], signatureString: Option[String]): Option[String] = {
    docSignatureForSymbol(typeFullName, memberName, signatureString).flatMap { sig => callRPC[Option[String]](docServer, DocUriReq(sig)) }
  }

  override def publicSymbolSearch(names: List[String], maxResults: Int): SymbolSearchResults = {
    callRPC[SymbolSearchResults](indexer, PublicSymbolSearchReq(names, maxResults))
  }

  override def usesOfSymAtPoint(file: File, point: Int): List[ERangePosition] = {
    callRPC[List[ERangePosition]](getAnalyzer, UsesOfSymbolAtPointReq(file, point))
  }

  override def typeAtPoint(file: File, range: OffsetRange): Option[TypeInfo] = {
    callRPC[Option[TypeInfo]](getAnalyzer, TypeAtPointReq(file, range))
  }

  override def inspectPackageByPath(path: String): Option[PackageInfo] = {
    callRPC[Option[PackageInfo]](getAnalyzer, InspectPackageByPathReq(path))
  }

  override def prepareRefactor(procId: Int, refactorDesc: RefactorDesc): Either[RefactorFailure, RefactorEffect] = {
    callRPC[Either[RefactorFailure, RefactorEffect]](getAnalyzer, PrepareRefactorReq(procId, null, refactorDesc, interactive = false))
  }

  override def execRefactor(procId: Int, refactorType: RefactorType): Either[RefactorFailure, RefactorResult] = {
    callRPC[Either[RefactorFailure, RefactorResult]](getAnalyzer, ExecRefactorReq(procId, refactorType))
  }

  override def cancelRefactor(procId: Int): Unit = {
    callVoidRPC(getAnalyzer, CancelRefactorReq(procId))
  }

  override def expandSelection(file: File, start: Int, stop: Int): FileRange = {
    callRPC[FileRange](getAnalyzer, ExpandSelectionReq(file, start, stop))
  }

  override def formatFiles(files: List[File]): Unit = {
    callVoidRPC(getAnalyzer, FormatSourceReq(files))
  }

  override def formatFile(fileInfo: SourceFileInfo): String = {
    callRPC[String](getAnalyzer, FormatOneSourceReq(fileInfo))
  }
}
