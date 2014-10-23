package org.ensime.protocol

import org.ensime.config.EnsimeConfig
import org.ensime.model._
import org.ensime.server._
import org.ensime.util.{ FileRange, SExp }

import scala.reflect.internal.util.RangePosition

trait RPCTarget {

  def rpcConnectionInfo(): ConnectionInfo
  def rpcShutdownServer(): Unit
  def rcpInitProject(confSExp: SExp): EnsimeConfig
  def rpcNotifyClientConnected(): Unit
  def rpcPeekUndo(): Either[String, Undo]
  def rpcExecUndo(undoId: Int): Either[String, UndoResult]
  def rpcReplConfig(): ReplConfig
  def rpcSymbolDesignations(f: String, start: Int, end: Int, requestedTypes: List[Symbol]): SymbolDesignations
  def rpcDebugStartVM(commandLine: String): DebugVmStatus
  def rpcDebugAttachVM(hostname: String, port: String): DebugVmStatus
  def rpcDebugStopVM(): Boolean
  def rpcDebugRun(): Boolean
  def rpcDebugContinue(threadId: Long): Boolean
  def rpcDebugBreak(file: String, line: Int): Unit
  def rpcDebugClearBreak(file: String, line: Int): Unit
  def rpcDebugClearAllBreaks(): Unit
  def rpcDebugListBreaks(): BreakpointList
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
  def rpcTypecheckFiles(fs: List[SourceFileInfo]): Unit
  def rpcRemoveFile(f: String): Unit
  def rpcUnloadAll(): Unit
  def rpcTypecheckAll(): Unit
  def rpcCompletionsAtPoint(f: String, point: Int, maxResults: Int, caseSens: Boolean, reload: Boolean): CompletionInfoList
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
  def rpcUsesOfSymAtPoint(f: String, point: Int): List[RangePosition]
  def rpcTypeAtPoint(f: String, range: OffsetRange): Option[TypeInfo]
  def rpcInspectPackageByPath(path: String): Option[PackageInfo]
  def rpcPrepareRefactor(procId: Int, refactorDesc: RefactorDesc): Either[RefactorFailure, RefactorEffect]
  def rpcExecRefactor(procId: Int, refactorType: Symbol): Either[RefactorFailure, RefactorResult]
  def rpcCancelRefactor(procId: Int): Unit
  def rpcExpandSelection(filename: String, start: Int, stop: Int): FileRange
  def rpcFormatFiles(filenames: List[String]): Unit
}

