package org.ensime.jerk

import java.io.File

import org.scalatest._

import org.ensime.api._

import spray.json._
import org.ensime.json._

import pimpathon.file._

class JerkFormatsSpec extends FlatSpec with Matchers
    with SprayJsonTestSupport with EnsimeTestData {
  import JerkEndpoints._

  // workaround the fact that we have tests on the contents of the
  // envelope, but marshallers at the higher level (really the tests
  // should be updated).
  def roundtrip(
    value: RpcRequest,
    via: String
  ): Unit = {
    val json = RpcRequestEnvelope(value, 999).toJson.asJsObject
    json.fields("req").compactPrint shouldBe via
    json.convertTo[RpcRequestEnvelope].req shouldBe value
  }

  def fileToWireString(file: File) = {
    val canonStr = file.canon.getAbsolutePath
    "\"" + canonStr.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
  }

  val file1_str = fileToWireString(file1)
  val file2_str = fileToWireString(file2)
  val file3_str = fileToWireString(file3)
  val file4_str = fileToWireString(file4)
  val file5_str = fileToWireString(file5)
  val abd_str = fileToWireString(abd)
  val symFile_str = fileToWireString(symFile)

  "Jerk Formats" should "roundtrip startup messages" in {
    roundtrip(
      ConnectionInfoReq: RpcRequest,
      """{"typehint":"ConnectionInfoReq"}"""
    )
  }

  it should "unmarshal RpcSearchRequests" in {
    roundtrip(
      PublicSymbolSearchReq(List("foo", "bar"), 10): RpcRequest,
      """{"typehint":"PublicSymbolSearchReq","keywords":["foo","bar"],"maxResults":10}"""
    )

    roundtrip(
      ImportSuggestionsReq(file1, 1, List("foo", "bar"), 10): RpcRequest,
      s"""{"point":1,"maxResults":10,"names":["foo","bar"],"typehint":"ImportSuggestionsReq","file":$file1_str}"""
    )
  }

  it should "unmarshal RpcAnalyserRequests" in {
    roundtrip(
      RemoveFileReq(file1): RpcRequest,
      s"""{"typehint":"RemoveFileReq","file":$file1_str}"""
    )

    roundtrip(
      TypecheckFileReq(sourceFileInfo): RpcRequest,
      s"""{"typehint":"TypecheckFileReq","fileInfo":{"file":$file1_str,"contents":"{/* code here */}","contentsIn":$file2_str}}"""
    )

    roundtrip(
      TypecheckFilesReq(List(file1, file2)): RpcRequest,
      s"""{"typehint":"TypecheckFilesReq","files":[$file1_str,$file2_str]}"""
    )

    roundtrip(
      UnloadAllReq: RpcRequest,
      """{"typehint":"UnloadAllReq"}"""
    )

    roundtrip(
      TypecheckAllReq: RpcRequest,
      """{"typehint":"TypecheckAllReq"}"""
    )

    roundtrip(
      FormatSourceReq(List(file1, file2)): RpcRequest,
      s"""{"typehint":"FormatSourceReq","files":[$file1_str,$file2_str]}"""
    )

    roundtrip(
      FormatOneSourceReq(sourceFileInfo): RpcRequest,
      s"""{"typehint":"FormatOneSourceReq","file":{"file":$file1_str,"contents":"{/* code here */}","contentsIn":$file2_str}}"""
    )

    roundtrip(
      DocUriAtPointReq(file1, OffsetRange(1, 10)): RpcRequest,
      s"""{"typehint":"DocUriAtPointReq","file":$file1_str,"point":{"from":1,"to":10}}"""
    )

    roundtrip(
      DocUriForSymbolReq("foo.bar", Some("Baz"), None): RpcRequest,
      """{"typehint":"DocUriForSymbolReq","typeFullName":"foo.bar","memberName":"Baz"}"""
    )

    roundtrip(
      CompletionsReq(sourceFileInfo, 10, 100, true, false): RpcRequest,
      s"""{"point":10,"maxResults":100,"typehint":"CompletionsReq","caseSens":true,"fileInfo":{"file":$file1_str,"contents":"{/* code here */}","contentsIn":$file2_str},"reload":false}"""
    )

    roundtrip(
      PackageMemberCompletionReq("foo", "bar"): RpcRequest,
      """{"typehint":"PackageMemberCompletionReq","path":"foo","prefix":"bar"}"""
    )

    roundtrip(
      CallCompletionReq(13): RpcRequest,
      """{"typehint":"CallCompletionReq","id":13}"""
    )

    roundtrip(
      UsesOfSymbolAtPointReq(file1, 100): RpcRequest,
      s"""{"typehint":"UsesOfSymbolAtPointReq","file":$file1_str,"point":100}"""
    )

    roundtrip(
      TypeByIdReq(13): RpcRequest,
      """{"typehint":"TypeByIdReq","id":13}"""
    )

    roundtrip(
      TypeByNameReq("foo.bar"): RpcRequest,
      """{"typehint":"TypeByNameReq","name":"foo.bar"}"""
    )

    roundtrip(
      TypeByNameAtPointReq("foo.bar", file1, OffsetRange(1, 10)): RpcRequest,
      s"""{"typehint":"TypeByNameAtPointReq","name":"foo.bar","file":$file1_str,"range":{"from":1,"to":10}}"""
    )

    roundtrip(
      TypeAtPointReq(file1, OffsetRange(1, 100)): RpcRequest,
      s"""{"typehint":"TypeAtPointReq","file":$file1_str,"range":{"from":1,"to":100}}"""
    )

    roundtrip(
      InspectTypeAtPointReq(file1, OffsetRange(1, 100)): RpcRequest,
      s"""{"typehint":"InspectTypeAtPointReq","file":$file1_str,"range":{"from":1,"to":100}}"""
    )

    roundtrip(
      InspectTypeByIdReq(13): RpcRequest,
      """{"typehint":"InspectTypeByIdReq","id":13}"""
    )

    roundtrip(
      InspectTypeByNameReq("foo.Bar"): RpcRequest,
      """{"typehint":"InspectTypeByNameReq","name":"foo.Bar"}"""
    )

    roundtrip(
      SymbolAtPointReq(file1, 101): RpcRequest,
      s"""{"typehint":"SymbolAtPointReq","file":$file1_str,"point":101}"""
    )

    roundtrip(
      SymbolByNameReq("foo.Bar", Some("baz"), None): RpcRequest,
      """{"typehint":"SymbolByNameReq","typeFullName":"foo.Bar","memberName":"baz"}"""
    )

    roundtrip(
      InspectPackageByPathReq("foo.bar"): RpcRequest,
      """{"typehint":"InspectPackageByPathReq","path":"foo.bar"}"""
    )

    roundtrip(
      PrepareRefactorReq(1, 'ignored, RenameRefactorDesc("bar", file1, 1, 100), false): RpcRequest,
      s"""{"tpe":"ignored","procId":1,"params":{"newName":"bar","typehint":"RenameRefactorDesc","end":100,"file":$file1_str,"start":1},"typehint":"PrepareRefactorReq","interactive":false}"""
    )

    roundtrip(
      ExecRefactorReq(1, RefactorType.Rename): RpcRequest,
      """{"typehint":"ExecRefactorReq","procId":1,"tpe":{"typehint":"Rename"}}"""
    )

    roundtrip(
      CancelRefactorReq(1): RpcRequest,
      """{"typehint":"CancelRefactorReq","procId":1}"""
    )

    roundtrip(
      SymbolDesignationsReq(
        file1, 1, 100,
        List(ObjectSymbol, ValSymbol)
      ): RpcRequest,
      s"""{"requestedTypes":[{"typehint":"ObjectSymbol"},{"typehint":"ValSymbol"}],"typehint":"SymbolDesignationsReq","end":100,"file":$file1_str,"start":1}"""
    )

    roundtrip(
      ExpandSelectionReq(file1, 100, 200): RpcRequest,
      s"""{"typehint":"ExpandSelectionReq","file":$file1_str,"start":100,"end":200}"""
    )

    roundtrip(
      ImplicitInfoReq(file1, OffsetRange(0, 123)): RpcRequest,
      s"""{"typehint":"ImplicitInfoReq","file":$file1_str,"range":{"from":0,"to":123}}"""
    )
  }

  it should "roundtrip RpcDebugRequests" in {
    roundtrip(
      DebugActiveVmReq: RpcRequest,
      """{"typehint":"DebugActiveVmReq"}"""
    )

    roundtrip(
      DebugStartReq("blah blah blah"): RpcRequest,
      """{"typehint":"DebugStartReq","commandLine":"blah blah blah"}"""
    )

    roundtrip(
      DebugAttachReq("mylovelyhorse", "13"): RpcRequest,
      """{"typehint":"DebugAttachReq","hostname":"mylovelyhorse","port":"13"}"""
    )

    roundtrip(
      DebugStopReq: RpcRequest,
      """{"typehint":"DebugStopReq"}"""
    )

    roundtrip(
      DebugSetBreakReq(file1, 13): RpcRequest,
      s"""{"typehint":"DebugSetBreakReq","file":$file1_str,"line":13}"""
    )

    roundtrip(
      DebugClearBreakReq(file1, 13): RpcRequest,
      s"""{"typehint":"DebugClearBreakReq","file":$file1_str,"line":13}"""
    )

    roundtrip(
      DebugClearAllBreaksReq: RpcRequest,
      """{"typehint":"DebugClearAllBreaksReq"}"""
    )

    roundtrip(
      DebugListBreakpointsReq: RpcRequest,
      """{"typehint":"DebugListBreakpointsReq"}"""
    )

    roundtrip(
      DebugRunReq: RpcRequest,
      """{"typehint":"DebugRunReq"}"""
    )

    roundtrip(
      DebugContinueReq(dtid): RpcRequest,
      """{"typehint":"DebugContinueReq","threadId":13}"""
    )

    roundtrip(
      DebugStepReq(dtid): RpcRequest,
      """{"typehint":"DebugStepReq","threadId":13}"""
    )

    roundtrip(
      DebugNextReq(dtid): RpcRequest,
      """{"typehint":"DebugNextReq","threadId":13}"""
    )

    roundtrip(
      DebugStepOutReq(dtid): RpcRequest,
      """{"typehint":"DebugStepOutReq","threadId":13}"""
    )

    roundtrip(
      DebugLocateNameReq(dtid, "foo"): RpcRequest,
      """{"typehint":"DebugLocateNameReq","threadId":13,"name":"foo"}"""
    )

    roundtrip(
      DebugValueReq(debugLocationArray): RpcRequest,
      """{"typehint":"DebugValueReq","loc":{"typehint":"DebugArrayElement","objectId":{"id":13},"index":14}}"""
    )

    roundtrip(
      DebugToStringReq(dtid, debugLocationArray): RpcRequest,
      """{"typehint":"DebugToStringReq","threadId":13,"loc":{"typehint":"DebugArrayElement","objectId":{"id":13},"index":14}}"""
    )

    roundtrip(
      DebugSetValueReq(debugLocationArray, "bar"): RpcRequest,
      """{"typehint":"DebugSetValueReq","loc":{"typehint":"DebugArrayElement","objectId":{"id":13},"index":14},"newValue":"bar"}"""
    )

    roundtrip(
      DebugBacktraceReq(dtid, 100, 200): RpcRequest,
      """{"typehint":"DebugBacktraceReq","threadId":13,"index":100,"count":200}"""
    )

  }

  it should "roundtrip EnsimeGeneralEvent as EnsimeEvent" in {
    roundtrip(
      SendBackgroundMessageEvent("ABCDEF", 1): EnsimeEvent,
      """{"typehint":"SendBackgroundMessageEvent","detail":"ABCDEF","code":1}"""
    )

    roundtrip(
      AnalyzerReadyEvent: EnsimeEvent,
      """{"typehint":"AnalyzerReadyEvent"}"""
    )

    roundtrip(
      FullTypeCheckCompleteEvent: EnsimeEvent,
      """{"typehint":"FullTypeCheckCompleteEvent"}"""
    )

    roundtrip(
      IndexerReadyEvent: EnsimeEvent,
      """{"typehint":"IndexerReadyEvent"}"""
    )

    roundtrip(
      NewScalaNotesEvent(
        isFull = false,
        List(new Note("foo.scala", "testMsg", NoteWarn, 50, 55, 77, 5))
      ): EnsimeEvent,
      """{"typehint":"NewScalaNotesEvent","isFull":false,"notes":[{"beg":50,"line":77,"col":5,"end":55,"file":"foo.scala","msg":"testMsg","severity":{"typehint":"NoteWarn"}}]}"""
    )

    roundtrip(
      ClearAllScalaNotesEvent: EnsimeEvent,
      """{"typehint":"ClearAllScalaNotesEvent"}"""
    )
  }

  it should "roundtrip DebugEvent as EnsimeEvent" in {
    roundtrip(
      DebugOutputEvent("XXX"): EnsimeEvent,
      """{"typehint":"DebugOutputEvent","body":"XXX"}"""
    )

    roundtrip(
      DebugStepEvent(DebugThreadId(207), "threadNameStr", sourcePos1.file, sourcePos1.line): EnsimeEvent,
      // why is the typehint not the first entry?
      s"""{"line":57,"typehint":"DebugStepEvent","file":$file1_str,"threadName":"threadNameStr","threadId":207}"""
    )

    roundtrip(
      DebugBreakEvent(DebugThreadId(209), "threadNameStr", sourcePos1.file, sourcePos1.line): EnsimeEvent,
      s"""{"line":57,"typehint":"DebugBreakEvent","file":$file1_str,"threadName":"threadNameStr","threadId":209}"""
    )

    roundtrip(
      DebugVMStartEvent: EnsimeEvent,
      """{"typehint":"DebugVMStartEvent"}"""
    )
    roundtrip(
      DebugVMDisconnectEvent: EnsimeEvent,
      """{"typehint":"DebugVMDisconnectEvent"}"""
    )
    roundtrip(
      DebugExceptionEvent(33L, dtid, "threadNameStr", Some(sourcePos1.file), Some(sourcePos1.line)): EnsimeEvent,
      s"""{"line":57,"exception":33,"typehint":"DebugExceptionEvent","file":$file1_str,"threadName":"threadNameStr","threadId":13}"""
    )
    roundtrip(
      DebugExceptionEvent(33L, dtid, "threadNameStr", None, None): EnsimeEvent,
      """{"typehint":"DebugExceptionEvent","exception":33,"threadId":13,"threadName":"threadNameStr"}"""
    )

    roundtrip(
      DebugThreadStartEvent(dtid): EnsimeEvent,
      """{"typehint":"DebugThreadStartEvent","threadId":13}"""
    )
    roundtrip(
      DebugThreadDeathEvent(dtid): EnsimeEvent,
      """{"typehint":"DebugThreadDeathEvent","threadId":13}"""
    )
  }

  it should "roundtrip DebugLocation" in {
    roundtrip(
      DebugObjectReference(57L): DebugLocation,
      """{"typehint":"DebugObjectReference","objectId":{"id":57}}"""
    )

    roundtrip(
      DebugArrayElement(DebugObjectId(58L), 2): DebugLocation,
      """{"typehint":"DebugArrayElement","objectId":{"id":58},"index":2}"""
    )

    roundtrip(
      DebugObjectField(DebugObjectId(58L), "fieldName"): DebugLocation,
      """{"typehint":"DebugObjectField","objectId":{"id":58},"field":"fieldName"}"""
    )

    roundtrip(
      DebugStackSlot(DebugThreadId(27), 12, 23): DebugLocation,
      """{"typehint":"DebugStackSlot","threadId":27,"frame":12,"offset":23}"""
    )
  }

  it should "roundtrip DebugValue" in {
    roundtrip(
      DebugPrimitiveValue("summaryStr", "typeNameStr"): DebugValue,
      """{"typehint":"DebugPrimitiveValue","summary":"summaryStr","typeName":"typeNameStr"}"""
    )

    roundtrip(
      DebugStringInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(5L)): DebugValue,
      """{"typehint":"DebugStringInstance","typeName":"typeNameStr","fields":[{"index":19,"name":"nameStr","typeName":"typeNameStr","summary":"summaryStr"}],"objectId":{"id":5},"summary":"summaryStr"}"""
    )

    roundtrip(
      DebugObjectInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(5L)): DebugValue,
      """{"typehint":"DebugObjectInstance","typeName":"typeNameStr","fields":[{"index":19,"name":"nameStr","typeName":"typeNameStr","summary":"summaryStr"}],"objectId":{"id":5},"summary":"summaryStr"}"""
    )

    roundtrip(
      DebugNullValue("typeNameStr"): DebugValue,
      """{"typehint":"DebugNullValue","typeName":"typeNameStr"}"""
    )

    roundtrip(
      DebugArrayInstance(3, "typeName", "elementType", DebugObjectId(5L)): DebugValue,
      """{"elementTypeName":"elementType","typehint":"DebugArrayInstance","typeName":"typeName","length":3,"objectId":{"id":5}}"""
    )

    roundtrip(
      debugClassField: DebugClassField,
      """{"index":19,"name":"nameStr","typeName":"typeNameStr","summary":"summaryStr"}"""
    )

    roundtrip(
      debugStackLocal1: DebugStackLocal,
      """{"index":3,"name":"name1","summary":"summary1","typeName":"type1"}"""
    )

    roundtrip(
      debugStackFrame: DebugStackFrame,
      s"""{"thisObjectId":{"id":7},"methodName":"method1","locals":[{"index":3,"name":"name1","summary":"summary1","typeName":"type1"},{"index":4,"name":"name2","summary":"summary2","typeName":"type2"}],"pcLocation":{"file":$file1_str,"line":57},"className":"class1","numArgs":4,"index":7}"""
    )

    roundtrip(
      DebugBacktrace(List(debugStackFrame), dtid, "thread1"): DebugBacktrace,
      s"""{"frames":[{"thisObjectId":{"id":7},"methodName":"method1","locals":[{"index":3,"name":"name1","summary":"summary1","typeName":"type1"},{"index":4,"name":"name2","summary":"summary2","typeName":"type2"}],"pcLocation":{"file":$file1_str,"line":57},"className":"class1","numArgs":4,"index":7}],"threadId":13,"threadName":"thread1"}"""
    )

    roundtrip(
      sourcePos1: SourcePosition,
      s"""{"typehint":"LineSourcePosition","file":$file1_str,"line":57}"""
    )
    roundtrip(
      sourcePos2: SourcePosition,
      s"""{"typehint":"LineSourcePosition","file":$file1_str,"line":59}"""
    )
    roundtrip(
      sourcePos3: SourcePosition,
      """{"typehint":"EmptySourcePosition"}"""
    )
    roundtrip(
      sourcePos4: SourcePosition,
      s"""{"typehint":"OffsetSourcePosition","file":$file1_str,"offset":456}"""
    )

    roundtrip(
      breakPoint1: Breakpoint,
      s"""{"file":$file1_str,"line":57}"""
    )

    roundtrip(
      BreakpointList(List(breakPoint1), List(breakPoint2)): BreakpointList,
      s"""{"active":[{"file":$file1_str,"line":57}],"pending":[{"file":$file1_str,"line":59}]}"""
    )

    roundtrip(
      DebugVmSuccess(): DebugVmStatus,
      """{"typehint":"DebugVmSuccess","status":"success"}"""
    )

    roundtrip(
      DebugVmError(303, "xxxx"): DebugVmStatus,
      """{"typehint":"DebugVmError","errorCode":303,"details":"xxxx","status":"error"}"""
    )
  }

  it should "roundtrip various informational types" in {
    roundtrip(
      note1: Note,
      """{"beg":23,"line":19,"col":8,"end":33,"file":"file1","msg":"note1","severity":{"typehint":"NoteError"}}"""
    )

    roundtrip(
      completionInfo: CompletionInfo,
      """{"name":"name","typeId":88,"typeSig":{"sections":[[["abc","def"],["hij","lmn"]]],"result":"ABC"},"relevance":90,"isCallable":false,"toInsert":"BAZ"}"""
    )

    roundtrip(
      completionInfo2: CompletionInfo,
      """{"name":"name2","typeId":90,"typeSig":{"sections":[[["abc","def"]]],"result":"ABC"},"relevance":91,"isCallable":true}"""
    )

    roundtrip(
      CompletionInfoList("fooBar", List(completionInfo)): CompletionInfoList,
      """{"prefix":"fooBar","completions":[{"name":"name","typeId":88,"typeSig":{"sections":[[["abc","def"],["hij","lmn"]]],"result":"ABC"},"relevance":90,"isCallable":false,"toInsert":"BAZ"}]}"""
    )

    roundtrip(
      new SymbolInfo("name", "localName", None, typeInfo, false, Some(2)): SymbolInfo,
      """{"name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"isCallable":false,"ownerTypeId":2}"""
    )

    roundtrip(
      new NamedTypeMemberInfo("typeX", typeInfo, None, None, DeclaredAs.Method): EntityInfo,
      """{"typehint":"NamedTypeMemberInfo","name":"typeX","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"declAs":{"typehint":"Method"}}"""
    )

    roundtrip(
      entityInfo: EntityInfo,
      """{"resultType":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"name":"Arrow1","paramSections":[{"params":[["ABC",{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}}]],"isImplicit":false}],"typehint":"ArrowTypeInfo","typeId":8}"""
    )

    roundtrip(
      typeInfo: EntityInfo,
      """{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}}"""
    )

    roundtrip(
      packageInfo: EntityInfo,
      """{"typehint":"PackageInfo","name":"name","fullName":"fullName","members":[]}"""
    )

    roundtrip(
      new CallCompletionInfo(typeInfo, List(paramSectionInfo)): CallCompletionInfo,
      """{"resultType":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"paramSections":[{"params":[["ABC",{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}}]],"isImplicit":false}]}"""
    )

    roundtrip(
      interfaceInfo: InterfaceInfo,
      """{"type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"viaView":"DEF"}"""
    )

    roundtrip(
      new TypeInspectInfo(typeInfo, Some(1), List(interfaceInfo)): TypeInspectInfo,
      """{"type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"companionId":1,"interfaces":[{"type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"viaView":"DEF"}],"infoType":"typeInspect"}"""
    )
  }

  it should "support search related responses" in {
    roundtrip(
      new SymbolSearchResults(List(methodSearchRes, typeSearchRes)): SymbolSearchResults,
      s"""{"syms":[{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":$abd_str,"line":10},"typehint":"MethodSearchResult","ownerName":"ownerStr","declAs":{"typehint":"Method"}},{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":$abd_str,"line":10},"typehint":"TypeSearchResult","declAs":{"typehint":"Trait"}}]}"""
    )

    roundtrip(
      new ImportSuggestions(List(List(methodSearchRes, typeSearchRes))): ImportSuggestions,
      s"""{"symLists":[[{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":$abd_str,"line":10},"typehint":"MethodSearchResult","ownerName":"ownerStr","declAs":{"typehint":"Method"}},{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":$abd_str,"line":10},"typehint":"TypeSearchResult","declAs":{"typehint":"Trait"}}]]}"""
    )

    roundtrip(
      methodSearchRes: SymbolSearchResult,
      s"""{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":$abd_str,"line":10},"typehint":"MethodSearchResult","ownerName":"ownerStr","declAs":{"typehint":"Method"}}"""
    )

    roundtrip(
      typeSearchRes: SymbolSearchResult,
      s"""{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":$abd_str,"line":10},"typehint":"TypeSearchResult","declAs":{"typehint":"Trait"}}"""
    )
  }

  it should "support ranges and semantic highlighting" in {
    roundtrip(
      new ERangePosition(batchSourceFile, 75, 70, 90): ERangePosition,
      s"""{"file":"/abc","offset":75,"start":70,"end":90}"""
    )

    roundtrip(
      FileRange("/abc", 7, 9): FileRange,
      s"""{"file":"/abc","start":7,"end":9}"""
    )

    roundtrip(
      SymbolDesignations(
        symFile, List(
        SymbolDesignation(7, 9, VarFieldSymbol),
        SymbolDesignation(11, 22, ClassSymbol)
      )
      ): SymbolDesignations,
      s"""{"file":$symFile_str,"syms":[{"start":7,"end":9,"symType":{"typehint":"VarFieldSymbol"}},{"start":11,"end":22,"symType":{"typehint":"ClassSymbol"}}]}"""
    )

    roundtrip(
      List(ImplicitConversionInfo(5, 6, symbolInfo)): List[ImplicitInfo],
      """[{"typehint":"ImplicitConversionInfo","start":5,"end":6,"fun":{"name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"isCallable":false,"ownerTypeId":2}}]"""
    )

    roundtrip(
      List(ImplicitParamInfo(5, 6, symbolInfo, List(symbolInfo, symbolInfo), true)): List[ImplicitInfo],
      """[{"params":[{"name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"isCallable":false,"ownerTypeId":2},{"name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"isCallable":false,"ownerTypeId":2}],"typehint":"ImplicitParamInfo","fun":{"name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"isCallable":false,"ownerTypeId":2},"funIsImplicit":true,"end":6,"start":5}]"""
    )

  }

  it should "refactoring messages" in {
    roundtrip(
      RefactorFailure(7, "message"): RefactorFailure,
      """{"procedureId":7,"reason":"message","status":"failure"}"""
    )

    roundtrip(
      refactorEffect: RefactorEffect,
      s"""{"procedureId":9,"refactorType":{"typehint":"AddImport"},"changes":[{"text":"aaa","typehint":"TextEdit","to":7,"from":5,"file":$file3_str}],"status":"success"}"""
    )

    roundtrip(
      refactorResult: RefactorResult,
      s"""{"procedureId":7,"refactorType":{"typehint":"AddImport"},"touchedFiles":[$file3_str,$file1_str],"status":"success"}"""
    )

  }
}
