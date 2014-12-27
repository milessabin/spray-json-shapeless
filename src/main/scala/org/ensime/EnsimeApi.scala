package org.ensime

import org.ensime.core._
import org.ensime.model._
import org.ensime.server.ConnectionInfo
import org.ensime.util.FileRange

trait EnsimeApi {

  def rpcConnectionInfo(): ConnectionInfo
  def rpcShutdownServer(): Unit
  def rpcNotifyClientReady(): Unit

  /**
   * Subscribe to async events from the project, replaying previously seen events if requested.
   * The first subscriber will get all undelivered events (subsequent subscribers do not).
   * @param handler The callback handler for events
   * @return True if caller is first subscriber, False otherwise
   */
  def rpcSubscribeAsync(handler: EnsimeEvent => Unit): Boolean
  def rpcPeekUndo(): Either[String, Undo]
  def rpcExecUndo(undoId: Int): Either[String, UndoResult]
  def rpcReplConfig(): ReplConfig
  def rpcSymbolDesignations(f: String, start: Int, end: Int, requestedTypes: Set[SourceSymbol]): SymbolDesignations
  def rpcDebugStartVM(commandLine: String): DebugVmStatus
  def rpcDebugAttachVM(hostname: String, port: String): DebugVmStatus
  def rpcDebugStopVM(): Boolean
  def rpcDebugRun(): Boolean
  def rpcDebugContinue(threadId: Long): Boolean
  def rpcDebugSetBreakpoint(file: String, line: Int): Unit
  def rpcDebugClearBreakpoint(file: String, line: Int): Unit
  def rpcDebugClearAllBreakpoints(): Unit
  def rpcDebugListBreakpoints(): BreakpointList
  def rpcDebugNext(threadId: Long): Boolean
  def rpcDebugStep(threadId: Long): Boolean
  def rpcDebugStepOut(threadId: Long): Boolean
  def rpcDebugLocateName(threadId: Long, name: String): Option[DebugLocation]
  def rpcDebugValue(loc: DebugLocation): Option[DebugValue]
  def rpcDebugToString(threadId: Long, loc: DebugLocation): Option[String]
  def rpcDebugSetValue(loc: DebugLocation, newValue: String): Boolean
  def rpcDebugBacktrace(threadId: Long, index: Int, count: Int): DebugBacktrace
  def rpcDebugActiveVM(): Boolean
  def rpcPatchSource(f: String, edits: List[PatchOp]): Unit
  def rpcTypecheckFiles(fs: List[SourceFileInfo], async: Boolean): Unit
  def rpcRemoveFile(f: String): Unit
  def rpcUnloadAll(): Unit
  def rpcTypecheckAll(): Unit
  def rpcCompletionsAtPoint(fileInfo: SourceFileInfo, point: Int, maxResults: Int, caseSens: Boolean): CompletionInfoList
  def rpcPackageMemberCompletion(path: String, prefix: String): List[CompletionInfo]
  def rpcInspectTypeAtPoint(f: String, range: OffsetRange): Option[TypeInspectInfo]
  def rpcInspectTypeById(id: Int): Option[TypeInspectInfo]
  def rpcInspectTypeByName(name: String): Option[TypeInspectInfo]
  def rpcSymbolAtPoint(f: String, point: Int): Option[SymbolInfo]
  def rpcMemberByName(typeFullName: String, memberName: String, memberIsType: Boolean): Option[SymbolInfo]
  def rpcTypeById(id: Int): Option[TypeInfo]
  def rpcTypeByName(name: String): Option[TypeInfo]
  def rpcTypeByNameAtPoint(name: String, f: String, range: OffsetRange): Option[TypeInfo]
  def rpcCallCompletion(id: Int): Option[CallCompletionInfo]
  def rpcImportSuggestions(f: String, point: Int, names: List[String], maxResults: Int): ImportSuggestions
  def rpcPublicSymbolSearch(names: List[String], maxResults: Int): SymbolSearchResults
  def rpcUsesOfSymAtPoint(f: String, point: Int): List[ERangePosition]
  def rpcTypeAtPoint(f: String, range: OffsetRange): Option[TypeInfo]
  def rpcInspectPackageByPath(path: String): Option[PackageInfo]
  def rpcPrepareRefactor(procId: Int, refactorDesc: RefactorDesc): Either[RefactorFailure, RefactorEffect]
  def rpcExecRefactor(procId: Int, refactorType: Symbol): Either[RefactorFailure, RefactorResult]
  def rpcCancelRefactor(procId: Int): Unit
  def rpcExpandSelection(filename: String, start: Int, stop: Int): FileRange
  def rpcFormatFiles(filenames: List[String]): Unit
  def rpcFormatFile(fileInfo: SourceFileInfo): String
}

