package org.ensime.server.protocol.swank

import java.io.File

import org.ensime.config._
import org.ensime.core._
import org.ensime.model._
import org.ensime.server.ConnectionInfo
import org.ensime.util._

import org.ensime.sexp._
import org.ensime.sexp.formats._

object SwankProtocolConversions {
  object Protocol extends DefaultSexpProtocol
    with SymbolAltFormat
    with OptionAltFormat
    with CanonFileFormat
    with FamilyFormats
    with CamelCaseToDashes
  import Protocol._

  /**
   * By default, S-Express uses the simple name of a class as the
   * typehint when resolving implementations of a sealed trait.
   * However, the ENSIME protocol uses custom typehints, which are
   * defined here - in combination with trait-specific typehint rules.
   */
  implicit val DebugObjectReferenceHint = TypeHint[DebugObjectReference](SexpSymbol("reference"))
  implicit val DebugArrayElementHint = TypeHint[DebugArrayElement](SexpSymbol("element"))
  implicit val DebugObjectFieldHint = TypeHint[DebugObjectField](SexpSymbol("field"))
  implicit val DebugStackSlotHint = TypeHint[DebugStackSlot](SexpSymbol("slot"))
  implicit val DebugPrimitiveHint = TypeHint[DebugPrimitiveValue](SexpSymbol("prim"))
  implicit val DebugObjectHint = TypeHint[DebugObjectInstance](SexpSymbol("obj"))
  implicit val DebugArrayHint = TypeHint[DebugArrayInstance](SexpSymbol("arr"))
  implicit val DebugStringHint = TypeHint[DebugStringInstance](SexpSymbol("str"))
  implicit val DebugNullHint = TypeHint[DebugNullValue](SexpSymbol("null"))
  implicit val NoteErrorHint = TypeHint[NoteError.type](SexpSymbol("error"))
  implicit val NoteWarnHint = TypeHint[NoteWarn.type](SexpSymbol("warn"))
  implicit val NoteInfoHint = TypeHint[NoteInfo.type](SexpSymbol("info"))
  implicit val DebugStepHint = TypeHint[DebugStepEvent](SexpSymbol("step"))
  implicit val DebugBreakHint = TypeHint[DebugBreakEvent](SexpSymbol("breakpoint"))
  implicit val DebugVMStartHint = TypeHint[DebugVMStartEvent.type](SexpSymbol("start"))
  implicit val DebugVMDisconnectHint = TypeHint[DebugVMDisconnectEvent.type](SexpSymbol("disconnect"))
  implicit val DebugExceptionHint = TypeHint[DebugExceptionEvent](SexpSymbol("exception"))
  implicit val DebugThreadStartHint = TypeHint[DebugThreadStartEvent](SexpSymbol("threadStart"))
  implicit val DebugThreadDeathHint = TypeHint[DebugThreadDeathEvent](SexpSymbol("threadDeath"))
  implicit val DebugOutputHint = TypeHint[DebugOutputEvent](SexpSymbol("output"))
  implicit val AnalyzerReadyHint = TypeHint[AnalyzerReadyEvent.type](SexpSymbol(":compiler-ready"))
  implicit val FullTypeCheckCompleteHint = TypeHint[FullTypeCheckCompleteEvent.type](SexpSymbol(":full-typecheck-finished"))
  implicit val IndexerReadyHint = TypeHint[IndexerReadyEvent.type](SexpSymbol(":indexer-ready"))
  implicit val CompilerRestartedHint = TypeHint[CompilerRestartedEvent.type](SexpSymbol(":compiler-restarted"))
  implicit val NewScalaNotesHint = TypeHint[NewScalaNotesEvent](SexpSymbol(":scala-notes"))
  implicit val ClearAllScalaNotesHint = TypeHint[ClearAllScalaNotesEvent.type](SexpSymbol(":clear-all-scala-notes"))
  implicit val SendBackgroundMessageHint = TypeHint[SendBackgroundMessageEvent](SexpSymbol(":background-message"))
  implicit val DebugHint = TypeHint[DebugEvent](SexpSymbol(":debug-event"))
  implicit val NamedTypeMemberHint = TypeHint[NamedTypeMemberInfo](SexpSymbol("named"))
  implicit val PackageHint = TypeHint[PackageInfo](SexpSymbol("package"))
  implicit val TypeInfoHint = TypeHint[TypeInfo](SexpSymbol("type"))
  implicit val ArrowTypeHint = TypeHint[ArrowTypeInfo](SexpSymbol("t"))
  implicit val BasicTypeHint = TypeHint[BasicTypeInfo](SexpSymbol("nil"))
  implicit val DebugVmSuccessHint = TypeHint[DebugVmSuccess](SexpSymbol("success"))
  implicit val DebugVmErrorHint = TypeHint[DebugVmError](SexpSymbol("error"))
  implicit val MethodSearchResultHint = TypeHint[MethodSearchResult](SexpSymbol("method"))
  implicit val TypeSearchResultHint = TypeHint[TypeSearchResult](SexpSymbol("type"))

  implicit val EmptySourcePositionHint = TypeHint[EmptySourcePosition](SexpSymbol("empty"))
  implicit val LineSourcePositionHint = TypeHint[LineSourcePosition](SexpSymbol("line"))
  implicit val OffsetSourcePositionHint = TypeHint[OffsetSourcePosition](SexpSymbol("offset"))
  implicit val TextEditHint = TypeHint[TextEdit](SexpSymbol("edit"))
  implicit val DeleteFileHint = TypeHint[DeleteFile](SexpSymbol("delete"))
  implicit val NewFileHint = TypeHint[NewFile](SexpSymbol("new"))

  /**
   * Alternative form for family formats that serialises the typehint
   * as a field value on the same level as the other parts (assumed to
   * be a SexpData), i.e. to match our legacy format.
   */
  abstract class TraitFormatAlt[T] extends SexpFormat[T] {
    val key = SexpSymbol(":type")
    protected def wrap[E](t: E)(
      implicit th: TypeHint[E], sf: SexpFormat[E]): Sexp = t.toSexp match {
      case SexpNil => SexpData(key -> th.hint)
      case SexpData(data) if !data.contains(key) =>
        SexpData(key -> th.hint :: data.toList)
      case SexpList(Nil) =>
        // special case: no param case classes
        SexpData(key -> th.hint)
      case other =>
        serializationError(s"expected ${th.hint}'s wrap to be SexpData, was $other")
    }
    final def read(sexp: Sexp): T = sexp match {
      case SexpData(map) if map.contains(key) =>
        map(key) match {
          case hint: SexpSymbol => read(hint, SexpData((map - key).toList))
          case not => deserializationError(not)
        }
      case x => deserializationError(x)
    }
    protected def read(hint: SexpSymbol, value: Sexp): T
  }

  /**
   * These implicits are required until shapeless stops println-ing when
   * singletonFormat is made implicit.
   */
  implicit val AnalyzerReadyEventFormat = singletonFormat[AnalyzerReadyEvent.type]
  implicit val FullTypeCheckCompleteEventFormat = singletonFormat[FullTypeCheckCompleteEvent.type]
  implicit val IndexerReadyEventFormat = singletonFormat[IndexerReadyEvent.type]
  implicit val CompilerRestartedEventFormat = singletonFormat[CompilerRestartedEvent.type]
  implicit val ClearAllScalaNotesEventFormat = singletonFormat[ClearAllScalaNotesEvent.type]
  implicit val DebugVMStartEventFormat = singletonFormat[DebugVMStartEvent.type]
  implicit val DebugVMDisconnectEventFormat = singletonFormat[DebugVMDisconnectEvent.type]

  /**
   * These implicit vals are actually optional - S-Express doesn't
   * *need* them - and exist only to help the compiler to resolve
   * various implicits without recomputing them. Runtime performance
   * is also improved by having these assigned to vals.
   */
  implicit val DebugObjectReferenceFormat = SexpFormat[DebugObjectReference]
  implicit val DebugArrayElementFormat = SexpFormat[DebugArrayElement]
  implicit val DebugObjectFieldFormat = SexpFormat[DebugObjectField]
  implicit val DebugStackSlotFormat = SexpFormat[DebugStackSlot]
  implicit val DebugPrimitiveValueFormat = SexpFormat[DebugPrimitiveValue]
  implicit val DebugObjectInstanceFormat = SexpFormat[DebugObjectInstance]
  implicit val DebugArrayInstanceFormat = SexpFormat[DebugArrayInstance]
  implicit val DebugStringInstanceFormat = SexpFormat[DebugStringInstance]
  implicit val DebugNullValueFormat = SexpFormat[DebugNullValue]
  implicit val DebugClassFieldFormat = SexpFormat[DebugClassField]
  implicit val DebugStackLocalFormat = SexpFormat[DebugStackLocal]
  implicit val DebugStackFrameFormat = SexpFormat[DebugStackFrame]
  implicit val DebugBacktraceFormat = SexpFormat[DebugBacktrace]
  implicit val OffsetSourcePositionFormat = SexpFormat[OffsetSourcePosition]
  implicit val LineSourcePositionFormat = SexpFormat[LineSourcePosition]
  implicit val ConnectionInfoFormat = SexpFormat[ConnectionInfo]
  implicit val SendBackgroundMessageEventFormat = SexpFormat[SendBackgroundMessageEvent]
  implicit val BreakpointFormat = SexpFormat[Breakpoint]
  implicit val BreakpointListFormat = SexpFormat[BreakpointList]
  implicit val ReplConfigFormat = SexpFormat[ReplConfig]
  implicit val PackageMemberInfoLightFormat = SexpFormat[PackageMemberInfoLight]
  implicit val FileRangeFormat = SexpFormat[FileRange]
  implicit val ERangePositionFormat = SexpFormat[ERangePosition]
  implicit val RefactorFailureFormat = SexpFormat[RefactorFailure]
  implicit val TextEditFormat = SexpFormat[TextEdit]
  implicit val NewFileFormat = SexpFormat[NewFile]
  implicit val DeleteFileFormat = SexpFormat[DeleteFile]
  implicit val RefactorResultFormat = SexpFormat[RefactorResult]
  implicit val DebugVmErrorFormat = SexpFormat[DebugVmError]
  implicit val EmptySourcePositionFormat = SexpFormat[EmptySourcePosition]

  implicit object DebugLocationFormat extends TraitFormatAlt[DebugLocation] {
    def write(dl: DebugLocation): Sexp = dl match {
      case dor: DebugObjectReference => wrap(dor)
      case dae: DebugArrayElement => wrap(dae)
      case dof: DebugObjectField => wrap(dof)
      case dss: DebugStackSlot => wrap(dss)
    }
    def read(hint: SexpSymbol, value: Sexp): DebugLocation = hint match {
      case s if s == DebugObjectReferenceHint.hint =>
        value.convertTo[DebugObjectReference]
      case s if s == DebugArrayElementHint.hint =>
        value.convertTo[DebugArrayElement]
      case s if s == DebugObjectFieldHint.hint =>
        value.convertTo[DebugObjectField]
      case s if s == DebugStackSlotHint.hint =>
        value.convertTo[DebugStackSlot]
      case _ => deserializationError(hint)
    }
  }

  implicit object DebugValueFormat extends TraitFormatAlt[DebugValue] {
    override val key = SexpSymbol(":val-type")
    def write(dv: DebugValue): Sexp = dv match {
      case dpv: DebugPrimitiveValue => wrap(dpv)
      case doi: DebugObjectInstance => wrap(doi)
      case dai: DebugArrayInstance => wrap(dai)
      case dsi: DebugStringInstance => wrap(dsi)
      case dnv: DebugNullValue => wrap(dnv)
    }
    def read(hint: SexpSymbol, value: Sexp): DebugValue = hint match {
      case s if s == DebugPrimitiveHint.hint =>
        value.convertTo[DebugPrimitiveValue]
      case s if s == DebugObjectHint.hint =>
        value.convertTo[DebugObjectInstance]
      case s if s == DebugArrayHint.hint =>
        value.convertTo[DebugArrayInstance]
      case s if s == DebugStringHint.hint =>
        value.convertTo[DebugStringInstance]
      case s if s == DebugNullHint.hint =>
        value.convertTo[DebugNullValue]
      case _ => deserializationError(hint)
    }
  }

  implicit object SourcePositionFormat extends TraitFormatAlt[SourcePosition] {
    def write(dl: SourcePosition): Sexp = dl match {
      case empty: EmptySourcePosition => wrap(empty)
      case line: LineSourcePosition => wrap(line)
      case offset: OffsetSourcePosition => wrap(offset)
    }
    def read(hint: SexpSymbol, value: Sexp): SourcePosition = hint match {
      case s if s == implicitly[TypeHint[EmptySourcePosition]].hint =>
        value.convertTo[EmptySourcePosition]
      case s if s == implicitly[TypeHint[LineSourcePosition]].hint =>
        value.convertTo[LineSourcePosition]
      case s if s == implicitly[TypeHint[OffsetSourcePosition]].hint =>
        value.convertTo[OffsetSourcePosition]
      case _ => deserializationError(hint)
    }
  }

  implicit object NoteSeverityFormat extends TraitFormat[NoteSeverity] {
    def write(ns: NoteSeverity): Sexp = ns match {
      case NoteError => NoteErrorHint.hint
      case NoteWarn => NoteWarnHint.hint
      case NoteInfo => NoteInfoHint.hint
    }
    def read(hint: SexpSymbol, value: Sexp): NoteSeverity = hint match {
      case s if s == NoteErrorHint.hint => NoteError
      case s if s == NoteWarnHint.hint => NoteWarn
      case s if s == NoteInfoHint.hint => NoteInfo
      case _ => deserializationError(hint)
    }
  }
  // must be defined after NoteSeverity
  implicit val NoteFormat = SexpFormat[Note]
  implicit val NewScalaNotesEventFormat = SexpFormat[NewScalaNotesEvent]

  implicit object DebugEventFormat extends TraitFormatAlt[DebugEvent] {
    def write(ee: DebugEvent): Sexp = ee match {
      case dse: DebugStepEvent => wrap(dse)
      case dbe: DebugBreakEvent => wrap(dbe)
      case DebugVMStartEvent => wrap(DebugVMStartEvent)
      case DebugVMDisconnectEvent => wrap(DebugVMDisconnectEvent)
      case dee: DebugExceptionEvent => wrap(dee)
      case dts: DebugThreadStartEvent => wrap(dts)
      case dtd: DebugThreadDeathEvent => wrap(dtd)
      case doe: DebugOutputEvent => wrap(doe)
    }
    def read(hint: SexpSymbol, value: Sexp): DebugEvent = hint match {
      case s if s == DebugStepHint.hint => value.convertTo[DebugStepEvent]
      case s if s == DebugBreakHint.hint => value.convertTo[DebugBreakEvent]
      case s if s == DebugVMStartHint.hint => DebugVMStartEvent
      case s if s == DebugVMDisconnectHint.hint => DebugVMDisconnectEvent
      case s if s == DebugExceptionHint.hint => value.convertTo[DebugExceptionEvent]
      case s if s == DebugThreadStartHint.hint => value.convertTo[DebugThreadStartEvent]
      case s if s == DebugThreadDeathHint.hint => value.convertTo[DebugThreadDeathEvent]
      case s if s == DebugOutputHint.hint => value.convertTo[DebugOutputEvent]
      case _ => deserializationError(hint)
    }
  }

  /**
   * This is a tricky one to retrofit:
   *  1. GeneralSwankEvents use the TraitFormat with custom hints
   *  2. DebugEvents use the TraitFormat with another TraitFormatAlt inside
   */
  implicit object EnsimeEventFormat extends TraitFormat[EnsimeEvent] {
    def write(ee: EnsimeEvent): Sexp = ee match {
      case e: AnalyzerReadyEvent.type => wrap(e)
      case e: FullTypeCheckCompleteEvent.type => wrap(e)
      case e: IndexerReadyEvent.type => wrap(e)
      case e: CompilerRestartedEvent.type => wrap(e)
      case nsc: NewScalaNotesEvent => wrap(nsc)
      case e: ClearAllScalaNotesEvent.type => wrap(e)
      case sbm: SendBackgroundMessageEvent => SexpList(
        // the odd one out...
        SendBackgroundMessageHint.hint,
        SexpNumber(sbm.code),
        sbm.detail.toSexp
      )
      case de: DebugEvent => wrap(de)
    }
    def read(hint: SexpSymbol, value: Sexp): EnsimeEvent = hint match {
      case s if s == AnalyzerReadyHint.hint => AnalyzerReadyEvent
      case s if s == FullTypeCheckCompleteHint.hint => FullTypeCheckCompleteEvent
      case s if s == IndexerReadyHint.hint => IndexerReadyEvent
      case s if s == CompilerRestartedHint.hint => CompilerRestartedEvent
      case s if s == NewScalaNotesHint.hint => value.convertTo[NewScalaNotesEvent]
      case s if s == ClearAllScalaNotesHint.hint => ClearAllScalaNotesEvent
      case s if s == SendBackgroundMessageHint.hint => ??? // unsupported
      case s if s == DebugHint.hint => value.convertTo[DebugEvent]
      case _ => deserializationError(hint)
    }
  }

  implicit object CompletionSignatureFormat extends SexpFormat[CompletionSignature] {
    private implicit val Tuple2Format = SexpFormat[(String, String)]
    def write(cs: CompletionSignature): Sexp =
      SexpList(cs.sections.toSexp, cs.result.toSexp)
    def read(sexp: Sexp): CompletionSignature = sexp match {
      case SexpList(a :: b :: Nil) => CompletionSignature(
        a.convertTo[List[List[(String, String)]]],
        b.convertTo[String]
      )
      case _ => deserializationError(sexp)
    }
  }
  // must be defined after CompletionSignatureFormat
  implicit val CompletionInfoFormat = SexpFormat[CompletionInfo]
  implicit val CompletionInfoListFormat = SexpFormat[CompletionInfoList]

  // watch out for recursive references here...
  implicit object EntityInfoFormat extends TraitFormatAlt[EntityInfo] {
    override val key = SexpSymbol(":info-type")
    def write(ti: EntityInfo): Sexp = ti match {
      case named: NamedTypeMemberInfo => wrap(named)
      case pack: PackageInfo => wrap(pack)
      case tpe: TypeInfo => wrap(tpe)
    }
    def read(hint: SexpSymbol, value: Sexp): EntityInfo = hint match {
      case s if s == NamedTypeMemberHint.hint => value.convertTo[NamedTypeMemberInfo]
      case s if s == PackageHint.hint => value.convertTo[PackageInfo]
      case s if s == TypeInfoHint.hint => value.convertTo[TypeInfo]
      case _ => deserializationError(hint)
    }
  }
  implicit object TypeInfoFormat extends TraitFormatAlt[TypeInfo] {
    // a bit weird, but that's how we've been doing it
    override val key = SexpSymbol(":arrow-type")
    def write(ti: TypeInfo): Sexp = ti match {
      case arrow: ArrowTypeInfo => wrap(arrow)
      case basic: BasicTypeInfo => wrap(basic)
    }
    def read(hint: SexpSymbol, value: Sexp): TypeInfo = hint match {
      case s if s == ArrowTypeHint.hint => value.convertTo[ArrowTypeInfo]
      case s if s == BasicTypeHint.hint => value.convertTo[BasicTypeInfo]
      case _ => deserializationError(hint)
    }
  }
  implicit def NamedTypeMemberInfoFormat = SexpFormat[NamedTypeMemberInfo]
  implicit def PackageInfoFormat = SexpFormat[PackageInfo]
  implicit def ParamSectionInfoFormat = SexpFormat[ParamSectionInfo]
  implicit def ArrowTypeInfoFormat = SexpFormat[ArrowTypeInfo]
  implicit def BasicTypeInfoFormat = SexpFormat[BasicTypeInfo]
  implicit def CallCompletionInfoFormat = SexpFormat[CallCompletionInfo]
  implicit def SymbolInfoFormat = SexpFormat[SymbolInfo]
  implicit def InterfaceInfoFormat = SexpFormat[InterfaceInfo]
  implicit def TypeInspectInfoFormat = SexpFormat[TypeInspectInfo]

  implicit object FileEditFormat extends TraitFormatAlt[FileEdit] {
    def write(ti: FileEdit): Sexp = ti match {
      case text: TextEdit => wrap(text)
      case nf: NewFile => wrap(nf)
      case df: DeleteFile => wrap(df)
    }
    def read(hint: SexpSymbol, value: Sexp): FileEdit = hint match {
      case t if t == implicitly[TypeHint[TextEdit]].hint => value.convertTo[TextEdit]
      case t if t == implicitly[TypeHint[NewFile]].hint => value.convertTo[NewFile]
      case t if t == implicitly[TypeHint[DeleteFile]].hint => value.convertTo[DeleteFile]
      case _ => deserializationError(hint)
    }
  }
  // must be after FileEditFormat
  implicit val RefactorEffectFormat = SexpFormat[RefactorEffect]

  // must be after SourcePosition
  implicit val TypeSearchResultFormat = SexpFormat[TypeSearchResult]
  implicit val MethodSearchResultFormat = SexpFormat[MethodSearchResult]
  implicit object SymbolSearchResultFormat extends TraitFormatAlt[SymbolSearchResult] {
    def write(ti: SymbolSearchResult): Sexp = ti match {
      case ts: TypeSearchResult => wrap(ts)
      case ms: MethodSearchResult => wrap(ms)
    }
    def read(hint: SexpSymbol, value: Sexp): SymbolSearchResult = hint match {
      case t if t == implicitly[TypeHint[TypeSearchResult]].hint => value.convertTo[TypeSearchResult]
      case t if t == implicitly[TypeHint[MethodSearchResult]].hint => value.convertTo[MethodSearchResult]
      case _ => deserializationError(hint)
    }
  }

  implicit object SymbolSearchResultsFormat extends SexpFormat[SymbolSearchResults] {
    def write(o: SymbolSearchResults): Sexp = o.syms.toSexp
    def read(sexp: Sexp): SymbolSearchResults = SymbolSearchResults(
      sexp.convertTo[List[SymbolSearchResult]]
    )
  }
  implicit object ImportSuggestionsFormat extends SexpFormat[ImportSuggestions] {
    def write(o: ImportSuggestions): Sexp = o.symLists.toSexp
    def read(sexp: Sexp): ImportSuggestions = ImportSuggestions(
      sexp.convertTo[List[List[SymbolSearchResult]]]
    )
  }

  // must be after FileEdit
  implicit val UndoFormat = SexpFormat[Undo]
  implicit val UndoResultFormat = SexpFormat[UndoResult]

  // TODO: don't use this lookup
  private val sourceSymbolMap = Map(
    "object" -> ObjectSymbol,
    "class" -> ClassSymbol,
    "trait" -> TraitSymbol,
    "package" -> PackageSymbol,
    "constructor" -> ConstructorSymbol,
    "importedName" -> ImportedNameSymbol,
    "typeParam" -> TypeParamSymbol,
    "param" -> ParamSymbol,
    "varField" -> VarFieldSymbol,
    "valField" -> ValFieldSymbol,
    "operator" -> OperatorFieldSymbol,
    "var" -> VarSymbol,
    "val" -> ValSymbol,
    "functionCall" -> FunctionCallSymbol
  )

  private val reverseSourceSymbolMap: Map[SourceSymbol, String] = sourceSymbolMap.map { case (name, symbol) => symbol -> name }

  def symbolToSourceSymbol(stringRep: String): Option[SourceSymbol] = sourceSymbolMap.get(stringRep)
  private def sourceSymbolToSymbol(sym: SourceSymbol): String = reverseSourceSymbolMap.get(sym).get

  // must be after SourceSymbol
  implicit object SymbolDesignationFormat extends SexpFormat[SymbolDesignation] {
    def write(o: SymbolDesignation): Sexp =
      SexpList(
        SexpSymbol(sourceSymbolToSymbol(o.symType)),
        o.start.toSexp,
        o.end.toSexp
      )
    def read(sexp: Sexp): SymbolDesignation = ???
  }
  implicit val SymbolDesignationsFormat = SexpFormat[SymbolDesignations]

  implicit object DebugVmStatusFormat extends TraitFormatAlt[DebugVmStatus] {
    def write(ti: DebugVmStatus): Sexp = ti match {
      case s: DebugVmSuccess => wrap(s)
      case e: DebugVmError => wrap(e)
    }
    def read(hint: SexpSymbol, value: Sexp): DebugVmStatus = hint match {
      case t if t == DebugVmSuccessHint.hint => value.convertTo[DebugVmSuccess]
      case t if t == DebugVmErrorHint.hint => value.convertTo[DebugVmError]
      case _ => deserializationError(hint)
    }
  }

  //////////////////////////////////////////////////////////////////////////
  // Everything below this line is to implement the legacy toWF API
  implicit class SExpressWireFormat(sexp: Sexp) extends WireFormat {
    def toWireString: String = sexp.compactPrint
    def withRpcReturn(callId: Int): WireFormat = new SExpressWireFormat(SexpList(
      SexpSymbol(":return"),
      SexpList(SexpSymbol(":ok"), sexp),
      SexpNumber(callId)
    ))
  }
  def toWF(dl: DebugLocation): WireFormat = dl.toSexp
  def toWF(dor: DebugObjectReference): WireFormat = dor.toSexp
  def toWF(dae: DebugArrayElement): WireFormat = dae.toSexp
  def toWF(dof: DebugObjectField): WireFormat = dof.toSexp
  def toWF(dss: DebugStackSlot): WireFormat = dss.toSexp
  def toWF(obj: DebugValue): WireFormat = obj.toSexp
  def toWF(obj: DebugNullValue): WireFormat = obj.toSexp
  def toWF(obj: DebugPrimitiveValue): WireFormat = obj.toSexp
  def toWF(obj: DebugObjectInstance): WireFormat = obj.toSexp
  def toWF(obj: DebugStringInstance): WireFormat = obj.toSexp
  def toWF(obj: DebugArrayInstance): WireFormat = obj.toSexp
  def toWF(obj: DebugClassField): WireFormat = obj.toSexp
  def toWF(obj: DebugStackLocal): WireFormat = obj.toSexp
  def toWF(obj: DebugStackFrame): WireFormat = obj.toSexp
  def toWF(obj: DebugBacktrace): WireFormat = obj.toSexp
  def toWF(pos: SourcePosition): WireFormat = pos.toSexp
  def toWF(pos: LineSourcePosition): WireFormat = pos.toSexp
  def toWF(pos: EmptySourcePosition): WireFormat = pos.toSexp
  def toWF(pos: OffsetSourcePosition): WireFormat = pos.toSexp
  def toWF(info: ConnectionInfo): WireFormat = info.toSexp
  def toWF(evt: EnsimeEvent): WireFormat = evt.toSexp
  def toWF(note: Note): WireFormat = note.toSexp
  def toWF(bp: Breakpoint): WireFormat = bp.toSexp
  def toWF(bps: BreakpointList): WireFormat = bps.toSexp
  def toWF(config: ReplConfig): WireFormat = config.toSexp
  def toWF(value: Boolean): WireFormat = value.toSexp
  def toWF(value: String): WireFormat = value.toSexp
  def toWF(cs: CompletionSignature): WireFormat = cs.toSexp
  def toWF(value: CompletionInfo): WireFormat = value.toSexp
  def toWF(value: CompletionInfoList): WireFormat = value.toSexp
  def toWF(value: PackageMemberInfoLight): WireFormat = value.toSexp
  def toWF(value: FileRange): WireFormat = value.toSexp
  def toWF(value: NamedTypeMemberInfo): WireFormat = value.toSexp
  def toWF(value: EntityInfo): WireFormat = value.toSexp
  def toWF(value: TypeInfo): WireFormat = value.toSexp
  def toWF(value: ParamSectionInfo): WireFormat = value.toSexp
  def toWF(value: SymbolInfo): WireFormat = value.toSexp
  def toWF(value: CallCompletionInfo): WireFormat = value.toSexp
  def toWF(value: TypeInspectInfo): WireFormat = value.toSexp
  def toWF(value: InterfaceInfo): WireFormat = value.toSexp
  def toWF(p: ERangePosition): WireFormat = p.toSexp
  def toWF(value: RefactorFailure): WireFormat = value.toSexp
  def toWF(value: RefactorEffect): WireFormat = value.toSexp
  def toWF(value: RefactorResult): WireFormat = value.toSexp
  def toWF(value: SymbolSearchResult): WireFormat = value.toSexp
  def toWF(value: SymbolSearchResults): WireFormat = value.toSexp
  def toWF(value: ImportSuggestions): WireFormat = value.toSexp
  def toWF(value: Undo): WireFormat = value.toSexp
  def toWF(value: UndoResult): WireFormat = value.toSexp
  def toWF(value: SymbolDesignations): WireFormat = value.toSexp
  def toWF(v: DebugVmStatus): WireFormat = v.toSexp

  // don't always work as expected by the legacy API
  def toWF[T: SexpFormat](els: Iterable[T]): WireFormat = els.toSexp
  def toWF[T: SexpFormat](els: Option[T]): WireFormat = els.toSexp
}
