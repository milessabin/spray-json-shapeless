package org.ensime.protocol

import org.ensime.config._
import org.ensime.model._
import org.ensime.server._
import org.ensime.util.{ FileRange, NoteList, Note, WireFormat }

import scala.reflect.internal.util.RangePosition

trait ProtocolConversions {
  def toWF(evt: ConnectionInfo): WireFormat
  def toWF(evt: SwankEvent): WireFormat

  def toWF(obj: DebugLocation): WireFormat
  def toWF(obj: DebugValue): WireFormat
  def toWF(evt: DebugNullValue): WireFormat
  def toWF(evt: DebugPrimitiveValue): WireFormat
  def toWF(evt: DebugClassField): WireFormat
  def toWF(obj: DebugStringInstance): WireFormat
  def toWF(evt: DebugObjectInstance): WireFormat
  def toWF(evt: DebugArrayInstance): WireFormat
  def toWF(evt: DebugStackLocal): WireFormat
  def toWF(evt: DebugStackFrame): WireFormat
  def toWF(evt: DebugBacktrace): WireFormat

  def toWF(pos: SourcePosition): WireFormat
  def toWF(config: Breakpoint): WireFormat
  def toWF(config: BreakpointList): WireFormat
  def toWF(config: EnsimeConfig): WireFormat
  def toWF(config: ReplConfig): WireFormat
  def toWF(value: Boolean): WireFormat
  def toWF(value: String): WireFormat
  def toWF(value: Note): WireFormat
  def toWF(notelist: NoteList): WireFormat

  def toWF(values: Iterable[WireFormat]): WireFormat
  def toWF(value: CompletionInfo): WireFormat
  def toWF(value: CompletionInfoList): WireFormat
  def toWF(value: PackageMemberInfoLight): WireFormat
  def toWF(value: SymbolInfo): WireFormat
  def toWF(value: NamedTypeMemberInfo): WireFormat
  def toWF(value: EntityInfo): WireFormat
  def toWF(value: TypeInfo): WireFormat
  def toWF(value: PackageInfo): WireFormat
  def toWF(value: CallCompletionInfo): WireFormat
  def toWF(value: InterfaceInfo): WireFormat
  def toWF(value: TypeInspectInfo): WireFormat
  def toWF(value: SymbolSearchResults): WireFormat
  def toWF(value: RangePosition): WireFormat
  def toWF(value: ImportSuggestions): WireFormat
  def toWF(value: SymbolSearchResult): WireFormat
  def toWF(value: FileRange): WireFormat
  def toWF(value: SymbolDesignations): WireFormat

  def toWF(value: RefactorFailure): WireFormat
  def toWF(value: RefactorEffect): WireFormat
  def toWF(value: RefactorResult): WireFormat
  def toWF(value: Undo): WireFormat
  def toWF(value: UndoResult): WireFormat
  // a wire format message representing null
  def wfNull: WireFormat
  def toWF(vmStatus: DebugVmStatus): WireFormat
  def toWF(method: MethodBytecode): WireFormat
}
