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

  /**
   *   Request the semantic classes of symbols in the given range. These classes are intended to be used for
   *   semantic highlighting.
   * Arguments:
   *   f source filename
   *   start The character offset of the start of the input range.
   *   End  The character offset of the end of the input range.
   *   requestedTypes The semantic classes in which we are interested. (@see SourceSymbol)
   * Return:
   *   SymbolDesignations The given
   */
  def rpcSymbolDesignations(f: String, start: Int, end: Int, requestedTypes: Set[SourceSymbol]): SymbolDesignations

  /**
   *   Patch the source with the given changes.
   *   @param f The file to patch
   *   @param edits The patches to apply to the file.
   */
  def rpcPatchSource(f: String, edits: List[PatchOp]): Unit

  def rpcTypecheckFiles(fs: List[SourceFileInfo], async: Boolean): Unit
  def rpcRemoveFile(f: String): Unit
  def rpcUnloadAll(): Unit
  def rpcTypecheckAll(): Unit
  def rpcCompletionsAtPoint(fileInfo: SourceFileInfo, point: Int, maxResults: Int, caseSens: Boolean): CompletionInfoList
  def rpcPackageMemberCompletion(path: String, prefix: String): List[CompletionInfo]

  /**
   * Return detailed type information about the item at the given file position.
   * @param fileName The source filename
   * @param range The range in the file to inspect.
   * @return Some(TypeInspectInfo) if the range represents a valid type, None otherwise
   */
  def rpcInspectTypeAtPoint(fileName: String, range: OffsetRange): Option[TypeInspectInfo]

  /**
   * Lookup detailed type description by typeId
   * @param typeId The id of the type to inspect (returned by other calls)
   * @return Some(TypeInspectInfo) if the typeId represents a valid type, None otherwise
   */
  def rpcInspectTypeById(typeId: Int): Option[TypeInspectInfo]

  /**
   * Lookup detailed type description by fully qualified class name
   * @param typeFQN The fully qualified type name to inspect
   * @return Some(TypeInspectInfo) if typeFQN represents a valid type, None otherwise
   */
  def rpcInspectTypeByName(typeFQN: String): Option[TypeInspectInfo]

  def rpcSymbolAtPoint(fileName: String, point: Int): Option[SymbolInfo]

  def rpcMemberByName(typeFullName: String, memberName: String, memberIsType: Boolean): Option[SymbolInfo]
  def rpcTypeById(id: Int): Option[TypeInfo]
  def rpcTypeByName(name: String): Option[TypeInfo]
  def rpcTypeByNameAtPoint(name: String, f: String, range: OffsetRange): Option[TypeInfo]
  def rpcCallCompletion(id: Int): Option[CallCompletionInfo]
  def rpcImportSuggestions(f: String, point: Int, names: List[String], maxResults: Int): ImportSuggestions
  def rpcDocSignatureAtPoint(f: String, point: OffsetRange): Option[DocSigPair]
  def rpcDocSignatureForSymbol(typeFullName: String, memberName: Option[String], memberTypeId: Option[Int]): Option[DocSigPair]
  def rpcDocUriAtPoint(f: String, point: OffsetRange): Option[String]
  def rpcDocUriForSymbol(typeFullName: String, memberName: Option[String], memberTypeId: Option[Int]): Option[String]
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

  def rpcDebugStartVM(commandLine: String): DebugVmStatus
  def rpcDebugAttachVM(hostname: String, port: String): DebugVmStatus
  def rpcDebugStopVM(): Boolean
  def rpcDebugRun(): Boolean
  def rpcDebugContinue(threadId: String): Boolean
  def rpcDebugSetBreakpoint(file: String, line: Int): Unit
  def rpcDebugClearBreakpoint(file: String, line: Int): Unit
  def rpcDebugClearAllBreakpoints(): Unit
  def rpcDebugListBreakpoints(): BreakpointList
  def rpcDebugNext(threadId: String): Boolean
  def rpcDebugStep(threadId: String): Boolean
  def rpcDebugStepOut(threadId: String): Boolean
  def rpcDebugLocateName(threadId: String, name: String): Option[DebugLocation]
  def rpcDebugValue(loc: DebugLocation): Option[DebugValue]
  def rpcDebugToString(threadId: String, loc: DebugLocation): Option[String]
  def rpcDebugSetValue(loc: DebugLocation, newValue: String): Boolean
  def rpcDebugBacktrace(threadId: String, index: Int, count: Int): DebugBacktrace
  def rpcDebugActiveVM(): Boolean
}

