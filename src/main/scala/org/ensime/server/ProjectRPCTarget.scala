package org.ensime.server

import java.io.File
import akka.actor.ActorRef
import org.ensime.config.EnsimeConfig
import org.ensime.model._
import org.ensime.protocol._
import org.ensime.protocol.ProtocolConst._
import org.ensime.protocol.{ ConnectionInfo, RPCTarget }
import org.ensime.util._

import scala.reflect.internal.util.RangePosition
import scalariform.astselect.AstSelector
import scalariform.formatter.ScalaFormatter
import scalariform.parser.ScalaParserException
import scalariform.utils.Range

import scala.concurrent.Await
import akka.pattern.ask
import scala.concurrent.duration._
import shapeless.syntax.typeable._
import shapeless.Typeable

trait ProjectRPCTarget extends RPCTarget { self: Project =>

  val defaultMaxWait: FiniteDuration = 30.seconds
  def callVoidRPC(target: ActorRef, request: RPCRequest, maxWait: FiniteDuration = defaultMaxWait): Unit = {
    callRPC[VoidResponse.type](target, request, maxWait)
  }

  def callRPC[R](target: ActorRef, request: RPCRequest, maxWait: FiniteDuration = defaultMaxWait)(implicit typ: Typeable[R]): R = {
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

  override def rcpInitProject(confSExp: SExp): EnsimeConfig = {
    // bit of a hack - simply return the init string right now until it goes away
    self.config
  }

  override def rpcNotifyClientConnected(): Unit = {
    actor ! ClientConnectedEvent
  }

  override def rpcPeekUndo(): Either[String, Undo] = {
    peekUndo()
  }

  override def rpcExecUndo(undoId: Int): Either[String, UndoResult] = {
    execUndo(undoId)
  }

  override def rpcReplConfig(): ReplConfig = {
    new ReplConfig(config.runtimeClasspath)
  }

  override def rpcSymbolDesignations(f: String, start: Int, end: Int,
    requestedTypes: List[Symbol]): SymbolDesignations = {
    val file: File = new File(f)
    callRPC[SymbolDesignations](getAnalyzer, SymbolDesignationsReq(file, start, end, requestedTypes))
  }

  override def rpcDebugStartVM(commandLine: String): DebugVmStatus = {
    callRPC[DebugVmStatus](acquireDebugger, DebugStartVMReq(commandLine))
  }

  override def rpcDebugAttachVM(hostname: String, port: String): DebugVmStatus = {
    callRPC[DebugVmStatus](acquireDebugger, DebugAttachVMReq(hostname, port))
  }

  override def rpcDebugStopVM(): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugStopVMReq)
  }

  override def rpcDebugRun(): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugRunReq)
  }

  override def rpcDebugContinue(threadId: Long): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugContinueReq(threadId))
  }

  override def rpcDebugBreak(file: String, line: Int): Unit = {
    callVoidRPC(acquireDebugger, DebugBreakReq(file, line))
  }

  override def rpcDebugClearBreak(file: String, line: Int): Unit = {
    callVoidRPC(acquireDebugger, DebugClearBreakReq(file, line))
  }

  override def rpcDebugClearAllBreaks(): Unit = {
    callVoidRPC(acquireDebugger, DebugClearAllBreaksReq)
  }

  override def rpcDebugListBreaks(): BreakpointList = {
    callRPC[BreakpointList](acquireDebugger, DebugListBreaksReq)
  }

  override def rpcDebugNext(threadId: Long): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugNextReq(threadId))
  }

  override def rpcDebugStep(threadId: Long): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugStepReq(threadId))
  }

  override def rpcDebugStepOut(threadId: Long): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugStepOutReq(threadId))
  }

  override def rpcDebugLocateName(threadId: Long, name: String): Option[DebugLocation] = {
    callRPC[Option[DebugLocation]](acquireDebugger, DebugLocateNameReq(threadId, name))
  }

  override def rpcDebugValue(loc: DebugLocation): Option[DebugValue] = {
    callRPC[Option[DebugValue]](acquireDebugger, DebugValueReq(loc))
  }

  override def rpcDebugToString(threadId: Long, loc: DebugLocation): Option[String] = {
    callRPC[Option[String]](acquireDebugger, DebugToStringReq(threadId, loc))
  }

  override def rpcDebugSetValue(loc: DebugLocation, newValue: String): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugSetValueReq(loc, newValue))
  }

  override def rpcDebugBacktrace(threadId: Long, index: Int, count: Int): DebugBacktrace = {
    callRPC[DebugBacktrace](acquireDebugger, DebugBacktraceReq(threadId, index, count))
  }

  override def rpcDebugActiveVM(): Boolean = {
    callRPC[Boolean](acquireDebugger, DebugActiveVMReq)
  }

  override def rpcPatchSource(f: String, edits: List[PatchOp]): Unit = {
    val file: File = new File(f)
    callVoidRPC(getAnalyzer, PatchSourceReq(file, edits))
  }

  override def rpcTypecheckFiles(fs: List[SourceFileInfo]): Unit = {
    callVoidRPC(getAnalyzer, ReloadFilesReq(fs))
  }

  override def rpcRemoveFile(f: String): Unit = {
    val file: File = new File(f)
    callVoidRPC(getAnalyzer, RemoveFileReq(file))
  }

  override def rpcUnloadAll() {
    callVoidRPC(getAnalyzer, UnloadAllReq)
  }

  override def rpcTypecheckAll(): Unit = {
    callVoidRPC(getAnalyzer, ReloadAllReq)
  }

  override def rpcCompletionsAtPoint(f: String, point: Int, maxResults: Int,
    caseSens: Boolean, reload: Boolean): CompletionInfoList = {

    callRPC[CompletionInfoList](getAnalyzer,
      CompletionsReq(new File(f), point, maxResults, caseSens, reload),
      30.seconds)
  }

  override def rpcPackageMemberCompletion(path: String, prefix: String): List[CompletionInfo] = {
    callRPC[List[CompletionInfo]](getAnalyzer, PackageMemberCompletionReq(path, prefix))
  }

  override def rpcInspectTypeAtPoint(f: String, range: OffsetRange): Option[TypeInspectInfo] = {
    callRPC[Option[TypeInspectInfo]](getAnalyzer, InspectTypeReq(new File(f), range))
  }

  override def rpcInspectTypeById(id: Int): Option[TypeInspectInfo] = {
    callRPC[Option[TypeInspectInfo]](getAnalyzer, InspectTypeByIdReq(id))
  }

  override def rpcInspectTypeByName(name: String): Option[TypeInspectInfo] = {
    callRPC[Option[TypeInspectInfo]](getAnalyzer, InspectTypeByNameReq(name))
  }

  override def rpcSymbolAtPoint(f: String, point: Int): Option[SymbolInfo] = {
    callRPC[Option[SymbolInfo]](getAnalyzer, SymbolAtPointReq(new File(f), point))
  }

  override def rpcMemberByName(typeFullName: String, memberName: String, memberIsType: Boolean): Option[SymbolInfo] = {
    callRPC[Option[SymbolInfo]](getAnalyzer, MemberByNameReq(typeFullName, memberName, memberIsType))
  }

  override def rpcTypeById(id: Int): Option[TypeInfo] = {
    callRPC[Option[TypeInfo]](getAnalyzer, TypeByIdReq(id))
  }

  override def rpcTypeByName(name: String): Option[TypeInfo] = {
    callRPC[Option[TypeInfo]](getAnalyzer, TypeByNameReq(name))
  }

  override def rpcTypeByNameAtPoint(name: String, f: String, range: OffsetRange): Option[TypeInfo] = {
    callRPC[Option[TypeInfo]](getAnalyzer, TypeByNameAtPointReq(name, new File(f), range))
  }

  override def rpcCallCompletion(id: Int): Option[CallCompletionInfo] = {
    callRPC[Option[CallCompletionInfo]](getAnalyzer, CallCompletionReq(id))
  }

  override def rpcImportSuggestions(f: String, point: Int, names: List[String], maxResults: Int): ImportSuggestions = {
    callRPC[ImportSuggestions](getIndexer, ImportSuggestionsReq(new File(f), point, names, maxResults))
  }

  override def rpcPublicSymbolSearch(names: List[String], maxResults: Int): SymbolSearchResults = {
    callRPC[SymbolSearchResults](getIndexer, PublicSymbolSearchReq(names, maxResults))
  }

  override def rpcUsesOfSymAtPoint(f: String, point: Int): List[RangePosition] = {
    callRPC[List[RangePosition]](getAnalyzer, UsesOfSymAtPointReq(new File(f), point))
  }

  override def rpcTypeAtPoint(f: String, range: OffsetRange): Option[TypeInfo] = {
    callRPC[Option[TypeInfo]](getAnalyzer, TypeAtPointReq(new File(f), range))
  }

  override def rpcInspectPackageByPath(path: String): Option[PackageInfo] = {
    callRPC[Option[PackageInfo]](getAnalyzer, InspectPackageByPathReq(path))
  }

  override def rpcPrepareRefactor(procId: Int, refactorDesc: RefactorDesc): Either[RefactorFailure, RefactorEffect] = {
    callRPC[Either[RefactorFailure, RefactorEffect]](getAnalyzer, RefactorPrepareReq(procId, refactorDesc))
  }

  override def rpcExecRefactor(procId: Int, refactorType: Symbol): Either[RefactorFailure, RefactorResult] = {
    callRPC[Either[RefactorFailure, RefactorResult]](getAnalyzer, RefactorExecReq(procId, refactorType))
  }

  override def rpcCancelRefactor(procId: Int): Unit = {
    callVoidRPC(getAnalyzer, RefactorCancelReq(procId))
  }

  override def rpcExpandSelection(filename: String, start: Int, stop: Int): FileRange = {
    try {
      FileUtils.readFile(new File(filename)) match {
        case Right(contents) =>
          val selectionRange = Range(start, stop - start)
          AstSelector.expandSelection(contents, selectionRange) match {
            case Some(range) => FileRange(filename, range.offset, range.offset + range.length)
            case _ =>
              FileRange(filename, start, stop)
          }
        case Left(e) => throw e
      }
    } catch {
      case e: ScalaParserException =>
        throw RPCError(ErrFormatFailed, "Could not parse broken syntax: " + e)
    }
  }

  override def rpcFormatFiles(filenames: List[String]): Unit = {
    val files = filenames.map { new File(_) }
    try {
      val changeList = files.map { f =>
        FileUtils.readFile(f) match {
          case Right(contents) =>
            val formatted = ScalaFormatter.format(contents, config.formattingPrefs)
            TextEdit(f, 0, contents.length, formatted)
          case Left(e) => throw e
        }
      }
      addUndo("Formatted source of " + filenames.mkString(", ") + ".", FileUtils.inverseEdits(changeList))
      FileUtils.writeChanges(changeList) match {
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
}
