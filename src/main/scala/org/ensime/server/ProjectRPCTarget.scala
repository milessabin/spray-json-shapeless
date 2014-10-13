package org.ensime.server

import java.io.File
import org.ensime.model._
import org.ensime.protocol._
import org.ensime.protocol.ProtocolConst._
import org.ensime.protocol.{ ConnectionInfo, RPCTarget }
import org.ensime.util._

import scalariform.astselect.AstSelector
import scalariform.formatter.ScalaFormatter
import scalariform.parser.ScalaParserException
import scalariform.utils.Range

trait ProjectRPCTarget extends RPCTarget { self: Project =>

  import protocol._
  import protocol.conversions._

  override def rpcConnectionInfo(callId: Int) {
    sendRPCReturn(conversions.toWF(new ConnectionInfo()), callId)
  }

  override def rpcShutdownServer(callId: Int) {
    sendRPCReturn(toWF(value = true), callId)
    shutdownServer()
  }

  override def rcpInitProject(confSExp: SExp, callId: Int) {
    // bit of a hack - simply return the init string right now until it goes away
    //    initProject(conf)
    sendRPCReturn(toWF(self.config), callId)
    actor ! ClientConnectedEvent
  }

  override def rpcPeekUndo(callId: Int) {
    peekUndo() match {
      case Right(result) => sendRPCReturn(toWF(result), callId)
      case Left(msg) => sendRPCError(ErrPeekUndoFailed, msg, callId)
    }
  }

  override def rpcExecUndo(undoId: Int, callId: Int) {
    execUndo(undoId) match {
      case Right(result) => sendRPCReturn(toWF(result), callId)
      case Left(msg) => sendRPCError(ErrExecUndoFailed, msg, callId)
    }
  }

  override def rpcReplConfig(callId: Int) {
    sendRPCReturn(toWF(new ReplConfig(config.runtimeClasspath)), callId)
  }

  override def rpcSymbolDesignations(f: String, start: Int, end: Int, requestedTypes: List[Symbol], callId: Int) {
    val file: File = new File(f)
    getAnalyzer ! RPCRequestEvent(SymbolDesignationsReq(file, start, end, requestedTypes), callId)
  }

  override def rpcMethodBytecode(f: String, line: Int, callId: Int) {
    getIndexer ! RPCRequestEvent(MethodBytecodeReq(new File(f).getName, line), callId)
  }

  override def rpcDebugStartVM(commandLine: String, callId: Int) {
    acquireDebugger ! RPCRequestEvent(DebugStartVMReq(commandLine), callId)
  }

  override def rpcDebugAttachVM(hostname: String, port: String, callId: Int) {
    acquireDebugger ! RPCRequestEvent(DebugAttachVMReq(hostname, port), callId)
  }

  override def rpcDebugStopVM(callId: Int) {
    acquireDebugger ! RPCRequestEvent(DebugStopVMReq, callId)
  }

  override def rpcDebugRun(callId: Int) {
    acquireDebugger ! RPCRequestEvent(DebugRunReq, callId)
  }

  override def rpcDebugContinue(threadId: Long, callId: Int) {
    acquireDebugger ! RPCRequestEvent(DebugContinueReq(threadId), callId)
  }

  override def rpcDebugBreak(file: String, line: Int, callId: Int) {
    acquireDebugger ! RPCRequestEvent(DebugBreakReq(file, line), callId)
  }

  override def rpcDebugClearBreak(file: String, line: Int, callId: Int) {
    acquireDebugger ! RPCRequestEvent(DebugClearBreakReq(file, line), callId)
  }

  override def rpcDebugClearAllBreaks(callId: Int) {
    acquireDebugger ! RPCRequestEvent(DebugClearAllBreaksReq, callId)
  }

  override def rpcDebugListBreaks(callId: Int) {
    acquireDebugger ! RPCRequestEvent(DebugListBreaksReq, callId)
  }

  override def rpcDebugNext(threadId: Long, callId: Int) {
    acquireDebugger ! RPCRequestEvent(DebugNextReq(threadId), callId)
  }

  override def rpcDebugStep(threadId: Long, callId: Int) {
    acquireDebugger ! RPCRequestEvent(DebugStepReq(threadId), callId)
  }

  override def rpcDebugStepOut(threadId: Long, callId: Int) {
    acquireDebugger ! RPCRequestEvent(DebugStepOutReq(threadId), callId)
  }

  override def rpcDebugLocateName(threadId: Long, name: String, callId: Int) {
    acquireDebugger ! RPCRequestEvent(DebugLocateNameReq(threadId, name), callId)
  }

  override def rpcDebugValue(loc: DebugLocation, callId: Int) {
    acquireDebugger ! RPCRequestEvent(DebugValueReq(loc), callId)
  }

  def rpcDebugToString(threadId: Long, loc: DebugLocation, callId: Int) {
    acquireDebugger ! RPCRequestEvent(DebugToStringReq(threadId, loc), callId)
  }

  override def rpcDebugSetValue(loc: DebugLocation, newValue: String, callId: Int) {
    acquireDebugger ! RPCRequestEvent(DebugSetValueReq(loc, newValue), callId)
  }

  override def rpcDebugBacktrace(threadId: Long, index: Int, count: Int, callId: Int) {
    acquireDebugger ! RPCRequestEvent(DebugBacktraceReq(threadId, index, count), callId)
  }

  override def rpcDebugActiveVM(callId: Int) {
    acquireDebugger ! RPCRequestEvent(DebugActiveVMReq, callId)
  }

  override def rpcPatchSource(f: String, edits: List[PatchOp], callId: Int) {
    val file: File = new File(f)
    getAnalyzer ! RPCRequestEvent(PatchSourceReq(file, edits), callId)
  }

  override def rpcTypecheckFiles(fs: List[SourceFileInfo], callId: Int) {
    getAnalyzer ! RPCRequestEvent(ReloadFilesReq(fs), callId)
  }

  override def rpcRemoveFile(f: String, callId: Int) {
    val file: File = new File(f)
    getAnalyzer ! RPCRequestEvent(RemoveFileReq(file), callId)
    sendRPCAckOK(callId)
  }

  override def rpcUnloadAll(callId: Int) {
    getAnalyzer ! RPCRequestEvent(UnloadAllReq, callId)
  }

  override def rpcTypecheckAll(callId: Int) {
    getAnalyzer ! RPCRequestEvent(ReloadAllReq, callId)
  }

  override def rpcCompletionsAtPoint(f: String, point: Int, maxResults: Int,
    caseSens: Boolean, reload: Boolean, callId: Int) {
    getAnalyzer ! RPCRequestEvent(
      CompletionsReq(new File(f), point, maxResults, caseSens, reload), callId)
  }

  override def rpcPackageMemberCompletion(path: String, prefix: String, callId: Int) {
    getAnalyzer ! RPCRequestEvent(PackageMemberCompletionReq(path, prefix), callId)
  }

  override def rpcInspectTypeAtPoint(f: String, range: OffsetRange, callId: Int) {
    getAnalyzer ! RPCRequestEvent(InspectTypeReq(new File(f), range), callId)
  }

  override def rpcInspectTypeById(id: Int, callId: Int) {
    getAnalyzer ! RPCRequestEvent(InspectTypeByIdReq(id), callId)
  }

  override def rpcInspectTypeByName(name: String, callId: Int) {
    getAnalyzer ! RPCRequestEvent(InspectTypeByNameReq(name), callId)
  }

  override def rpcSymbolAtPoint(f: String, point: Int, callId: Int) {
    getAnalyzer ! RPCRequestEvent(SymbolAtPointReq(new File(f), point), callId)
  }

  override def rpcMemberByName(typeFullName: String, memberName: String, memberIsType: Boolean, callId: Int) {
    getAnalyzer ! RPCRequestEvent(MemberByNameReq(typeFullName, memberName, memberIsType), callId)
  }

  override def rpcTypeById(id: Int, callId: Int) {
    getAnalyzer ! RPCRequestEvent(TypeByIdReq(id), callId)
  }

  override def rpcTypeByName(name: String, callId: Int) {
    getAnalyzer ! RPCRequestEvent(TypeByNameReq(name), callId)
  }

  override def rpcTypeByNameAtPoint(name: String, f: String, range: OffsetRange, callId: Int) {
    getAnalyzer ! RPCRequestEvent(TypeByNameAtPointReq(name, new File(f), range), callId)
  }

  override def rpcCallCompletion(id: Int, callId: Int) {
    getAnalyzer ! RPCRequestEvent(CallCompletionReq(id), callId)
  }

  override def rpcImportSuggestions(f: String, point: Int, names: List[String], maxResults: Int, callId: Int) {
    getAnalyzer ! RPCRequestEvent(ImportSuggestionsReq(new File(f), point, names, maxResults), callId)
  }

  override def rpcPublicSymbolSearch(names: List[String], maxResults: Int, callId: Int) {
    getAnalyzer ! RPCRequestEvent(PublicSymbolSearchReq(names, maxResults), callId)
  }

  override def rpcUsesOfSymAtPoint(f: String, point: Int, callId: Int) {
    getAnalyzer ! RPCRequestEvent(UsesOfSymAtPointReq(new File(f), point), callId)
  }

  override def rpcTypeAtPoint(f: String, range: OffsetRange, callId: Int) {
    getAnalyzer ! RPCRequestEvent(TypeAtPointReq(new File(f), range), callId)
  }

  override def rpcInspectPackageByPath(path: String, callId: Int) {
    getAnalyzer ! RPCRequestEvent(InspectPackageByPathReq(path), callId)
  }

  override def rpcPrepareRefactor(procId: Int, refactorDesc: RefactorDesc, interactive: Boolean, callId: Int) {
    getAnalyzer ! RPCRequestEvent(RefactorPrepareReq(procId, refactorDesc, interactive), callId)
  }

  override def rpcExecRefactor(procId: Int, refactorType: Symbol, callId: Int) {
    getAnalyzer ! RPCRequestEvent(RefactorExecReq(procId, refactorType), callId)
  }

  override def rpcCancelRefactor(procId: Int, callId: Int) {
    getAnalyzer ! RPCRequestEvent(RefactorCancelReq(procId), callId)
  }

  override def rpcExpandSelection(filename: String, start: Int, stop: Int, callId: Int) {
    try {
      FileUtils.readFile(new File(filename)) match {
        case Right(contents) =>
          val selectionRange = Range(start, stop - start)
          AstSelector.expandSelection(contents, selectionRange) match {
            case Some(range) => sendRPCReturn(
              toWF(FileRange(filename, range.offset, range.offset + range.length)), callId)
            case _ => sendRPCReturn(
              toWF(FileRange(filename, start, stop)), callId)
          }
        case Left(e) => throw e
      }
    } catch {
      case e: ScalaParserException =>
        sendRPCError(ErrFormatFailed, "Could not parse broken syntax: " + e, callId)
    }
  }

  override def rpcFormatFiles(filenames: List[String], callId: Int) {
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
        case Right(_) => sendRPCAckOK(callId)
        case Left(e) =>
          sendRPCError(ErrFormatFailed, "Could not write any formatting changes: " + e, callId)
      }
    } catch {
      case e: ScalaParserException =>
        sendRPCError(ErrFormatFailed, "Cannot format broken syntax: " + e, callId)
    }
  }
}
