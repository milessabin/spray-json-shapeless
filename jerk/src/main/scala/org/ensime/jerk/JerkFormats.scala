package org.ensime.jerk

import java.io.File
import scala.util._

import spray.json._
import org.ensime.json._
import shapeless._

import org.ensime.api._

import pimpathon.file._

object JerkFormats extends DefaultJsonProtocol with FamilyFormats {
  // wtf?? why is this needed, why does it even work? Miles??
  implicit val symbolFormat = SymbolJsonFormat

  // FIXME: (reminder) canonise File with a shapeless Poly1 in endpoints
  implicit object FileFormat extends JsonFormat[File] {
    def read(j: JsValue): File = j match {
      case JsString(path) => file(path)
      case other => unexpectedJson[File](other)
    }
    def write(f: File): JsValue = JsString(f.getPath)
  }

  implicit object DebugThreadIdFormat extends JsonFormat[DebugThreadId] {
    def read(j: JsValue): DebugThreadId = j match {
      case JsNumber(id) => new DebugThreadId(id.longValue)
      case other => unexpectedJson[DebugThreadId](other)
    }
    def write(dtid: DebugThreadId): JsValue = JsNumber(dtid.id)
  }

  // some of the case classes use the keyword `type`, so we need a better default
  override implicit def coproductHint[T: Typeable]: CoproductHint[T] = new FlatCoproductHint[T]("typehint")

  val RpcRequestEnvelopeFormat = JsonFormat[RpcRequestEnvelope]

  // FIXME: introduce a family tree that means we don't have to have
  // so many specific converters. e.g. a single sealed family in and
  // and single sealed family out
  val RpcErrorFormat = JsonFormat[RpcError]
  val EnsimeEventFormat = RootJsonFormat[EnsimeEvent]

  // everything below this line is crap, slowing down the compile
  val ConnectionInfoFormat = RootJsonFormat[ConnectionInfo]
  val NamedTypeMemberInfoFormat = RootJsonFormat[NamedTypeMemberInfo]
  val TypeInfoFormat = RootJsonFormat[TypeInfo]
  val SourcePositionFormat = RootJsonFormat[SourcePosition]
  val DebugLocationFormat = RootJsonFormat[DebugLocation]
  val DebugValueFormat = RootJsonFormat[DebugValue]
  val DebugClassFieldFormat = RootJsonFormat[DebugClassField]
  val DebugStackLocalFormat = RootJsonFormat[DebugStackLocal]
  val DebugStackFrameFormat = RootJsonFormat[DebugStackFrame]
  val DebugBacktraceFormat = RootJsonFormat[DebugBacktrace]
  val BreakpointFormat = RootJsonFormat[Breakpoint]
  val BreakpointListFormat = RootJsonFormat[BreakpointList]
  val DebugVmStatusFormat = RootJsonFormat[DebugVmStatus]
  val NoteFormat = RootJsonFormat[Note]
  val CompletionInfoFormat = RootJsonFormat[CompletionInfo]
  val CompletionInfoListFormat = RootJsonFormat[CompletionInfoList]
  val SymbolInfoFormat = RootJsonFormat[SymbolInfo]
  val EntityInfoFormat = RootJsonFormat[EntityInfo]
  val CallCompletionInfoFormat = RootJsonFormat[CallCompletionInfo]
  val InterfaceInfoFormat = RootJsonFormat[InterfaceInfo]
  val TypeInspectInfoFormat = RootJsonFormat[TypeInspectInfo]
  val SymbolSearchResultFormat = RootJsonFormat[SymbolSearchResult]
  val SymbolSearchResultsFormat = RootJsonFormat[SymbolSearchResults]
  val ImportSuggestionsFormat = RootJsonFormat[ImportSuggestions]
  val ERangePositionFormat = RootJsonFormat[ERangePosition]
  val FileRangeFormat = RootJsonFormat[FileRange]
  val SymbolDesignationsFormat = RootJsonFormat[SymbolDesignations]
  val RefactorFailureFormat = RootJsonFormat[RefactorFailure]
  val RefactorEffectFormat = RootJsonFormat[RefactorEffect]
  val RefactorResultFormat = RootJsonFormat[RefactorResult]
  val ListERangePositionFormat = RootJsonFormat[List[ERangePosition]]
  val ListImplicitInfoFormat = RootJsonFormat[List[ImplicitInfo]]

}

// Isolated just to help me see exactly what is needed
object JerkEndpoints {
  implicit val RpcRequestEnvelopeFormat = JerkFormats.RpcRequestEnvelopeFormat

  implicit val RpcErrorFormat = JerkFormats.RpcErrorFormat
  implicit val EnsimeEventFormat = JerkFormats.EnsimeEventFormat
  // *sigh*
  implicit val ConnectionInfoFormat = JerkFormats.ConnectionInfoFormat
  implicit val NamedTypeMemberInfoFormat = JerkFormats.NamedTypeMemberInfoFormat
  implicit val TypeInfoFormat = JerkFormats.TypeInfoFormat
  implicit val SourcePositionFormat = JerkFormats.SourcePositionFormat
  implicit val DebugLocationFormat = JerkFormats.DebugLocationFormat
  implicit val DebugValueFormat = JerkFormats.DebugValueFormat
  implicit val DebugClassFieldFormat = JerkFormats.DebugClassFieldFormat
  implicit val DebugStackLocalFormat = JerkFormats.DebugStackLocalFormat
  implicit val DebugStackFrameFormat = JerkFormats.DebugStackFrameFormat
  implicit val DebugBacktraceFormat = JerkFormats.DebugBacktraceFormat
  implicit val BreakpointFormat = JerkFormats.BreakpointFormat
  implicit val BreakpointListFormat = JerkFormats.BreakpointListFormat
  implicit val DebugVmStatusFormat = JerkFormats.DebugVmStatusFormat
  implicit val NoteFormat = JerkFormats.NoteFormat
  implicit val CompletionInfoFormat = JerkFormats.CompletionInfoFormat
  implicit val CompletionInfoListFormat = JerkFormats.CompletionInfoListFormat
  implicit val SymbolInfoFormat = JerkFormats.SymbolInfoFormat
  implicit val EntityInfoFormat = JerkFormats.EntityInfoFormat
  implicit val CallCompletionInfoFormat = JerkFormats.CallCompletionInfoFormat
  implicit val InterfaceInfoFormat = JerkFormats.InterfaceInfoFormat
  implicit val TypeInspectInfoFormat = JerkFormats.TypeInspectInfoFormat
  implicit val SymbolSearchResultFormat = JerkFormats.SymbolSearchResultFormat
  implicit val SymbolSearchResultsFormat = JerkFormats.SymbolSearchResultsFormat
  implicit val ImportSuggestionsFormat = JerkFormats.ImportSuggestionsFormat
  implicit val ERangePositionFormat = JerkFormats.ERangePositionFormat
  implicit val FileRangeFormat = JerkFormats.FileRangeFormat
  implicit val SymbolDesignationsFormat = JerkFormats.SymbolDesignationsFormat
  implicit val RefactorFailureFormat = JerkFormats.RefactorFailureFormat
  implicit val RefactorEffectFormat = JerkFormats.RefactorEffectFormat
  implicit val RefactorResultFormat = JerkFormats.RefactorResultFormat
  implicit val ListERangePositionFormat = JerkFormats.ListERangePositionFormat
  implicit val ListImplicitInfoFormat = JerkFormats.ListImplicitInfoFormat

  import JerkFormats.BooleanJsonFormat
  import JerkFormats.StringJsonFormat

  // WORKAROUND not having a sealed family for RPC responses
  def unhappyFamily(msg: Any): JsValue = msg match {
    // we need an Ensime wrapper for these primitive response types
    case b: Boolean => b.toJson
    case s: String => s.toJson
    case list: List[_] if list.forall(_.isInstanceOf[ERangePosition]) =>
      list.asInstanceOf[List[ERangePosition]].toJson
    case list: List[_] if list.forall(_.isInstanceOf[ImplicitInfo]) =>
      list.asInstanceOf[List[ImplicitInfo]].toJson

    case VoidResponse => false.toJson
    case value: ConnectionInfo => value.toJson
    case value: NamedTypeMemberInfo => value.toJson
    case value: TypeInfo => value.toJson
    case value: EntityInfo => value.toJson
    case value: SymbolSearchResult => value.toJson
    case value: DebugVmStatus => value.toJson

    case value: SourcePosition => value.toJson
    case value: DebugLocation => value.toJson
    case value: DebugValue => value.toJson
    case value: DebugClassField => value.toJson
    case value: DebugStackLocal => value.toJson
    case value: DebugStackFrame => value.toJson
    case value: DebugBacktrace => value.toJson
    case value: Breakpoint => value.toJson
    case value: BreakpointList => value.toJson
    case value: Note => value.toJson
    case value: CompletionInfo => value.toJson
    case value: CompletionInfoList => value.toJson
    case value: SymbolInfo => value.toJson
    case value: CallCompletionInfo => value.toJson
    case value: InterfaceInfo => value.toJson
    case value: TypeInspectInfo => value.toJson
    case value: SymbolSearchResults => value.toJson
    case value: ImportSuggestions => value.toJson
    case value: ERangePosition => value.toJson
    case value: FileRange => value.toJson
    case value: SymbolDesignations => value.toJson
    case value: RefactorFailure => value.toJson
    case value: RefactorEffect => value.toJson
    case value: RefactorResult => value.toJson

    case _ => throw new IllegalArgumentException(s"$msg is not a valid JERK response")
  }

}
