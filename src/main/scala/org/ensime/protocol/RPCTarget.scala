package org.ensime.protocol

import org.ensime.model._
import org.ensime.server.RefactorDesc
import org.ensime.util.SExp

trait RPCTarget {

  def rpcConnectionInfo(callId: Int): Unit
  def rpcShutdownServer(callId: Int): Unit

  def rcpInitProject(confSExp: SExp, callId: Int): Unit
  def rpcPeekUndo(callId: Int): Unit
  def rpcExecUndo(undoId: Int, callId: Int): Unit
  def rpcReplConfig(callId: Int): Unit

  def rpcSymbolDesignations(f: String, start: Int, end: Int, requestedTypes: List[Symbol], callId: Int): Unit
  def rpcMethodBytecode(f: String, line: Int, callId: Int)
  def rpcDebugStartVM(commandLine: String, callId: Int)
  def rpcDebugAttachVM(hostname: String, port: String, callId: Int)
  def rpcDebugStopVM(callId: Int)
  def rpcDebugRun(callId: Int)
  def rpcDebugContinue(threadId: Long, callId: Int)
  def rpcDebugBreak(file: String, line: Int, callId: Int)
  def rpcDebugClearBreak(file: String, line: Int, callId: Int)
  def rpcDebugClearAllBreaks(callId: Int)
  def rpcDebugListBreaks(callId: Int)
  def rpcDebugNext(threadId: Long, callId: Int)
  def rpcDebugStep(threadId: Long, callId: Int)
  def rpcDebugStepOut(threadId: Long, callId: Int)
  def rpcDebugLocateName(threadId: Long, name: String, callId: Int)
  def rpcDebugValue(loc: DebugLocation, callId: Int)
  def rpcDebugToString(threadId: Long, loc: DebugLocation, callId: Int)
  def rpcDebugSetValue(loc: DebugLocation, newValue: String, callId: Int)
  def rpcDebugBacktrace(threadId: Long, index: Int, count: Int, callId: Int)
  def rpcDebugActiveVM(callId: Int)
  def rpcPatchSource(f: String, edits: List[PatchOp], callId: Int)
  def rpcTypecheckFiles(fs: List[SourceFileInfo], callId: Int)
  def rpcRemoveFile(f: String, callId: Int)
  def rpcUnloadAll(callId: Int)
  def rpcTypecheckAll(callId: Int)
  def rpcCompletionsAtPoint(f: String, point: Int, maxResults: Int, caseSens: Boolean, reload: Boolean, callId: Int)
  def rpcPackageMemberCompletion(path: String, prefix: String, callId: Int)
  def rpcInspectTypeAtPoint(f: String, range: OffsetRange, callId: Int)

  def rpcInspectTypeById(id: Int, callId: Int)
  def rpcInspectTypeByName(name: String, callId: Int)

  def rpcSymbolAtPoint(f: String, point: Int, callId: Int)

  def rpcMemberByName(typeFullName: String, memberName: String, memberIsType: Boolean, callId: Int)

  def rpcTypeById(id: Int, callId: Int)

  def rpcTypeByName(name: String, callId: Int)

  def rpcTypeByNameAtPoint(name: String, f: String, range: OffsetRange, callId: Int)

  def rpcCallCompletion(id: Int, callId: Int)

  def rpcImportSuggestions(f: String, point: Int, names: List[String], maxResults: Int, callId: Int)
  def rpcPublicSymbolSearch(names: List[String], maxResults: Int, callId: Int)

  def rpcUsesOfSymAtPoint(f: String, point: Int, callId: Int)

  def rpcTypeAtPoint(f: String, range: OffsetRange, callId: Int)

  def rpcInspectPackageByPath(path: String, callId: Int)

  def rpcPrepareRefactor(procId: Int, refactorDesc: RefactorDesc, interactive: Boolean, callId: Int)

  def rpcExecRefactor(procId: Int, refactorType: Symbol, callId: Int)

  def rpcCancelRefactor(procId: Int, callId: Int)

  def rpcExpandSelection(filename: String, start: Int, stop: Int, callId: Int)

  def rpcFormatFiles(filenames: List[String], callId: Int)
}

