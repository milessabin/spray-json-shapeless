package org.ensime.test

import java.io.File

import org.ensime.config.{ ProjectConfig, ReplConfig }
import org.ensime.indexer.{ Op, MethodBytecode }
import org.ensime.model._
import org.ensime.server._
import org.scalatest.FunSpec
import org.scalatest.ShouldMatchers
import org.ensime.util._

import scala.reflect.internal.util._

class SwankProtocolConversionsSpec extends FunSpec with ShouldMatchers {

  // some useful example components
  val packageInfo = new PackageInfo("name", "fullName", List())
  val typeInfo = new TypeInfo("type1", 7, 'type1, "FOO.type1", List(), List(), NoPosition, Some(8))
  val paramSectionInfo = new ParamSectionInfo(List(("ABC", typeInfo)), false)
  val interfaceInfo = new InterfaceInfo(typeInfo, Some("DEF"))

  val entityInfo: EntityInfo = new ArrowTypeInfo("Arrow1", 8, typeInfo, List(paramSectionInfo))

  val completionInfo = new CompletionInfo("name",
    new CompletionSignature(List(List(("abc", "def"), ("hij", "lmn"))), "ABC"), 88, false, 90, Some("BAZ"))

  val methodSearchRes = MethodSearchResult("abc", "a", 'abcd, Some("abd", 27), "ownerStr")
  val typeSearchRes = TypeSearchResult("abc", "a", 'abcd, Some("abd", 27))

  val note1 = new Note("file1", "note1", 2, 23, 33, 19, 8)
  val note2 = new Note("file1", "note2", 1, 23, 33, 19, 8)

  val sourcePos1 = new SourcePosition(CanonFile("/abc/def"), 57)
  val sourcePos2 = new SourcePosition(CanonFile("/abc/def"), 59)
  val breakPoint1 = new Breakpoint(sourcePos1)
  val breakPoint2 = new Breakpoint(sourcePos2)

  val debugStackLocal1 = DebugStackLocal(3, "name1", "type1", "summary1")
  val debugStackLocal2 = DebugStackLocal(4, "name2", "type2", "summary2")

  val debugStackFrame = DebugStackFrame(7, List(debugStackLocal1, debugStackLocal2), 4, "class1", "method1", sourcePos1, 7)

  val debugClassField = DebugClassField(19, "nameStr", "typeNameStr", "summaryStr")

  describe("SwankProtocolConversionsSpec") {

    import org.ensime.protocol.SwankProtocol._
    it("should encode all message types correctly") {
      assert(toWF(SendBackgroundMessageEvent(1, Some("ABCDEF"))).toWireString === """(:background-message 1 "ABCDEF")""")
      assert(toWF(SendBackgroundMessageEvent(1, None)).toWireString === "(:background-message 1 nil)")
      assert(toWF(AnalyzerReadyEvent()).toWireString === "(:compiler-ready)")
      assert(toWF(FullTypeCheckCompleteEvent()).toWireString === "(:full-typecheck-finished)")
      assert(toWF(IndexerReadyEvent()).toWireString === "(:indexer-ready)")

      assert(toWF(NewNotesEvent('abc, NoteList(full = false,
        List(new Note("foo.scala", "testMsg", 1, 50, 55, 77, 5))))).toWireString === """(:java-notes (:is-full nil :notes ((:severity warn :msg "testMsg" :beg 50 :end 55 :line 77 :col 5 :file "foo.scala"))))""")
      assert(toWF(NewNotesEvent('scala, NoteList(full = false,
        List(new Note("foo.scala", "testMsg", 1, 50, 55, 77, 5))))).toWireString === """(:scala-notes (:is-full nil :notes ((:severity warn :msg "testMsg" :beg 50 :end 55 :line 77 :col 5 :file "foo.scala"))))""")

      assert(toWF(ClearAllNotesEvent('scala)).toWireString === "(:clear-all-scala-notes)")
      assert(toWF(ClearAllNotesEvent('java)).toWireString === "(:clear-all-java-notes)")

      // toWF(evt: DebugEvent): WireFormat
      assert(toWF(DebugOutputEvent("XXX")).toWireString === """(:debug-event (:type output :body "XXX"))""")
      assert(toWF(DebugStepEvent(207L, "threadNameStr", sourcePos1)).toWireString ===
        """(:debug-event (:type step :thread-id "207" :thread-name "threadNameStr" :file "/abc/def" :line 57))""")
      assert(toWF(DebugBreakEvent(209L, "threadNameStr", sourcePos1)).toWireString ===
        """(:debug-event (:type breakpoint :thread-id "209" :thread-name "threadNameStr" :file "/abc/def" :line 57))""")
      assert(toWF(DebugVMDeathEvent()).toWireString === """(:debug-event (:type death))""")
      assert(toWF(DebugVMStartEvent()).toWireString === """(:debug-event (:type start))""")
      assert(toWF(DebugVMDisconnectEvent()).toWireString === """(:debug-event (:type disconnect))""")

      assert(toWF(DebugExceptionEvent(33L, 209L, "threadNameStr", Some(sourcePos1))).toWireString ===
        """(:debug-event (:type exception :exception "33" :thread-id "209" :thread-name "threadNameStr" :file "/abc/def" :line 57))""")

      assert(toWF(DebugThreadStartEvent(907L)).toWireString === """(:debug-event (:type threadStart :thread-id "907"))""")
      assert(toWF(DebugThreadDeathEvent(907L)).toWireString === """(:debug-event (:type threadDeath :thread-id "907"))""")

      // toWF(obj: DebugLocation)
      assert(toWF(DebugObjectReference(57L).asInstanceOf[DebugLocation]).toWireString ===
        """(:type reference :object-id "57")""")
      assert(toWF(DebugArrayElement(58L, 2).asInstanceOf[DebugLocation]).toWireString ===
        """(:type element :object-id "58" :index 2)""")
      assert(toWF(DebugObjectField(58L, "fieldName").asInstanceOf[DebugLocation]).toWireString ===
        """(:type field :object-id "58" :field "fieldName")""")
      assert(toWF(DebugStackSlot(27L, 12, 23).asInstanceOf[DebugLocation]).toWireString ===
        """(:type slot :thread-id "27" :frame 12 :offset 23)""")

      // toWF(obj: DebugValue)

      // toWF(evt: DebugPrimitiveValue)
      assert(toWF(DebugPrimitiveValue("summaryStr", "typeNameStr").asInstanceOf[DebugValue]).toWireString ===
        """(:val-type prim :summary "summaryStr" :type-name "typeNameStr")""")

      // toWF(evt: DebugClassField)
      assert(toWF(debugClassField).toWireString === """(:index 19 :name "nameStr" :summary "summaryStr" :type-name "typeNameStr")""")

      // toWF(obj: DebugStringInstance)
      assert(toWF(DebugStringInstance("summaryStr", List(debugClassField), "typeNameStr", 5L).asInstanceOf[DebugValue]).toWireString ===
        """(:val-type str :summary "summaryStr" :fields ((:index 19 :name "nameStr" :summary "summaryStr" :type-name "typeNameStr")) :type-name "typeNameStr" :object-id "5")""")

      // toWF(evt: DebugObjectInstance)
      assert(toWF(DebugObjectInstance("summaryStr", List(debugClassField), "typeNameStr", 5L).asInstanceOf[DebugValue]).toWireString ===
        """(:val-type obj :fields ((:index 19 :name "nameStr" :summary "summaryStr" :type-name "typeNameStr")) :type-name "typeNameStr" :object-id "5")""")

      // toWF(evt: DebugNullValue)
      assert(toWF(DebugNullValue("typeNameStr").asInstanceOf[DebugValue]).toWireString ===
        """(:val-type null :type-name "typeNameStr")""")

      // toWF(evt: DebugArrayInstance)
      assert(toWF(DebugArrayInstance(3, "typeName", "elementType", 5L).asInstanceOf[DebugValue]).toWireString ===
        """(:val-type arr :length 3 :type-name "typeName" :element-type-name "elementType" :object-id "5")""")

      // toWF(evt: DebugStackLocal)
      assert(toWF(debugStackLocal1).toWireString === """(:index 3 :name "name1" :summary "summary1" :type-name "type1")""")

      // toWF(evt: DebugStackFrame)
      assert(toWF(debugStackFrame).toWireString === """(:index 7 :locals ((:index 3 :name "name1" :summary "summary1" :type-name "type1") (:index 4 :name "name2" :summary "summary2" :type-name "type2")) :num-args 4 :class-name "class1" :method-name "method1" :pc-location (:file "/abc/def" :line 57) :this-object-id "7")""")

      // toWF(evt: DebugBacktrace)
      assert(toWF(DebugBacktrace(List(debugStackFrame), 17, "thread1")).toWireString === """(:frames ((:index 7 :locals ((:index 3 :name "name1" :summary "summary1" :type-name "type1") (:index 4 :name "name2" :summary "summary2" :type-name "type2")) :num-args 4 :class-name "class1" :method-name "method1" :pc-location (:file "/abc/def" :line 57) :this-object-id "7")) :thread-id "17" :thread-name "thread1")""")

      // toWF(pos: SourcePosition)
      assert(toWF(sourcePos1).toWireString === """(:file "/abc/def" :line 57)""")

      // toWF(bp: Breakpoint)
      assert(toWF(breakPoint1).toWireString === """(:file "/abc/def" :line 57)""")

      // toWF(config: BreakpointList)
      assert(toWF(BreakpointList(List(breakPoint1), List(breakPoint2))).toWireString === """(:active ((:file "/abc/def" :line 57)) :pending ((:file "/abc/def" :line 59)))""")

      // toWF(config: ProjectConfig)
      assert(toWF(ProjectConfig()).toWireString === """(:project-name nil :source-roots ())""")
      assert(toWF(ProjectConfig(name = Some("Project1"), sourceRoots = List(CanonFile("/test/test/")))).toWireString === """(:project-name "Project1" :source-roots ("/test/test"))""")

      // toWF(config: ReplConfig)
      assert(toWF(new ReplConfig("classpath;classpath")).toWireString === """(:classpath "classpath;classpath")""")

      // toWF(value: Boolean)
      assert(toWF(value = true).toWireString === """t""")
      assert(toWF(value = false).toWireString === """nil""")

      // toWF(value: String)
      assert(toWF("ABC").toWireString === """"ABC"""")

      // toWF(value: Note)
      assert(toWF(note1).toWireString === """(:severity error :msg "note1" :beg 23 :end 33 :line 19 :col 8 :file "file1")""")

      //  toWF(notelist: NoteList)
      assert(toWF(new NoteList(true, List(note1, note2))).toWireString === """(:is-full t :notes ((:severity error :msg "note1" :beg 23 :end 33 :line 19 :col 8 :file "file1") (:severity warn :msg "note2" :beg 23 :end 33 :line 19 :col 8 :file "file1")))""")

      // toWF(values: Iterable[WireFormat])
      assert(toWF(List(toWF(value = true), toWF(value = false), toWF("ABC"))).toWireString === """(t nil "ABC")""")

      // toWF(value: CompletionInfo)
      assert(toWF(completionInfo).toWireString === """(:name "name" :type-sig (((("abc" "def") ("hij" "lmn"))) "ABC") :type-id 88 :to-insert "BAZ")""")

      // toWF(value: CompletionInfoList)
      assert(toWF(CompletionInfoList("fooBar", List(completionInfo))).toWireString === """(:prefix "fooBar" :completions ((:name "name" :type-sig (((("abc" "def") ("hij" "lmn"))) "ABC") :type-id 88 :to-insert "BAZ")))""")
      // toWF(value: PackageMemberInfoLight)
      assert(toWF(new PackageMemberInfoLight("packageName")).toWireString === """(:name "packageName")""")
      // toWF(value: SymbolInfo)
      assert(toWF(new SymbolInfo("name", "localName", NoPosition, typeInfo, false, Some(2))).toWireString === """(:name "name" :local-name "localName" :type (:name "type1" :type-id 7 :full-name "FOO.type1" :decl-as type1 :outer-type-id 8) :owner-type-id 2)""")
      // toWF(value: NamedTypeMemberInfoLight)
      assert(toWF(new NamedTypeMemberInfoLight("typeMember1", "I(I)", 77, false)).toWireString === """(:name "typeMember1" :type-sig "I(I)" :type-id 77)""")

      // toWF(value: NamedTypeMemberInfo)
      assert(toWF(new NamedTypeMemberInfo("typeX", typeInfo, NoPosition, 'abcd)).toWireString === """(:name "typeX" :type (:name "type1" :type-id 7 :full-name "FOO.type1" :decl-as type1 :outer-type-id 8) :decl-as abcd)""")

      // toWF(value: EntityInfo)
      assert(toWF(entityInfo).toWireString === """(:name "Arrow1" :type-id 8 :arrow-type t :result-type (:name "type1" :type-id 7 :full-name "FOO.type1" :decl-as type1 :outer-type-id 8) :param-sections ((:params (("ABC" (:name "type1" :type-id 7 :full-name "FOO.type1" :decl-as type1 :outer-type-id 8))))))""")

      // toWF(value: TypeInfo)
      assert(toWF(typeInfo).toWireString === """(:name "type1" :type-id 7 :full-name "FOO.type1" :decl-as type1 :outer-type-id 8)""")

      // toWF(value: PackageInfo)
      assert(toWF(packageInfo).toWireString === """(:name "name" :info-type package :full-name "fullName")""")

      // toWF(value: CallCompletionInfo)
      assert(toWF(new CallCompletionInfo(typeInfo, List(paramSectionInfo))).toWireString === """(:result-type (:name "type1" :type-id 7 :full-name "FOO.type1" :decl-as type1 :outer-type-id 8) :param-sections ((:params (("ABC" (:name "type1" :type-id 7 :full-name "FOO.type1" :decl-as type1 :outer-type-id 8))))))""")

      // TODO this bit needs a lot more tests
      // toWF(value: InterfaceInfo)
      assert(toWF(interfaceInfo).toWireString === """(:type (:name "type1" :type-id 7 :full-name "FOO.type1" :decl-as type1 :outer-type-id 8) :via-view "DEF")""")
      // toWF(value: TypeInspectInfo)
      assert(toWF(new TypeInspectInfo(typeInfo, Some(1), List(interfaceInfo))).toWireString === """(:type (:name "type1" :type-id 7 :full-name "FOO.type1" :decl-as type1 :outer-type-id 8) :info-type typeInspect :companion-id 1 :interfaces ((:type (:name "type1" :type-id 7 :full-name "FOO.type1" :decl-as type1 :outer-type-id 8) :via-view "DEF")))""")

      // toWF(value: SymbolSearchResults)
      assert(toWF(new SymbolSearchResults(List(methodSearchRes, typeSearchRes))).toWireString === """((:name "abc" :local-name "a" :decl-as abcd :pos (:file "abd" :offset 27) :owner-name "ownerStr") (:name "abc" :local-name "a" :decl-as abcd :pos (:file "abd" :offset 27)))""")

      // toWF(value: ImportSuggestions)
      assert(toWF(new ImportSuggestions(List(List(methodSearchRes, typeSearchRes)))).toWireString ===
        """(((:name "abc" :local-name "a" :decl-as abcd :pos (:file "abd" :offset 27) :owner-name "ownerStr") (:name "abc" :local-name "a" :decl-as abcd :pos (:file "abd" :offset 27))))""")

      // toWF(value: SymbolSearchResult)
      assert(toWF(methodSearchRes).toWireString === """(:name "abc" :local-name "a" :decl-as abcd :pos (:file "abd" :offset 27) :owner-name "ownerStr")""")
      assert(toWF(typeSearchRes).toWireString === """(:name "abc" :local-name "a" :decl-as abcd :pos (:file "abd" :offset 27))""")

      // toWF(Position)
      assert(toWF(NoPosition).toWireString === """nil""")
      assert(toWF(FakePos("ABC")).toWireString === """nil""")
      // TODO add a test with fake file creator
      //      assert(toWF(new OffsetPosition("ABC")).toWireString === """nil""")

      // to test the real file path needto have a temp file as it is read
      // new BatchSourceFile(new PlainFile(Path("/abc")))
      // TODO add a fake file creator
      assert(toWF(new RangePosition(NoSourceFile, 70, 75, 90)).toWireString === """(:file "<no file>" :offset 75 :start 70 :end 90)""")

      assert(toWF(FileRange("/abc", 7, 9)).toWireString === """(:file "/abc" :start 7 :end 9)""")

      assert(toWF(SymbolDesignations("/abc", List(
        SymbolDesignation(7, 9, 'abc),
        SymbolDesignation(11, 22, 'def)
      ))).toWireString === """(:file "/abc" :syms ((abc 7 9) (def 11 22)))""")

      assert(toWF(RefactorFailure(7, "message")).toWireString === """(:procedure-id 7 :status failure :reason "message")""")

      assert(toWF(new RefactorEffect {
        val procedureId = 9
        val refactorType = 'add
        val changes = List(TextEdit(new File("/foo/abc"), 5, 7, "aaa"))
      }).toWireString === """(:procedure-id 9 :refactor-type add :status success :changes ((:file "/foo/abc" :text "aaa" :from 5 :to 7)))""")

      assert(toWF(new RefactorResult {
        val refactorType = 'abc
        val procedureId = 7
        val touched = List(new File("/foo/abc"), new File("/abc/def"))
      }).toWireString === """(:procedure-id 7 :refactor-type abc :status success :touched-files ("/foo/abc" "/abc/def"))""")

      assert(toWF(Undo(3, "Undoing stuff", List(
        TextEdit(new File("/foo/abc"), 5, 7, "aaa"),
        NewFile(new File("/foo/def"), "xxxxx"),
        DeleteFile(new File("/foo/hij"), "zzzz")
      ))).toWireString === """(:id 3 :changes ((:file "/foo/abc" :text "aaa" :from 5 :to 7) (:file "/foo/def" :text "xxxxx" :from 0 :to 4) (:file "/foo/hij" :text "zzzz" :from 0 :to 3)) :summary "Undoing stuff")""")

      assert(toWF(UndoResult(7, List(new File("/foo/abc"), new File("/foo/def")))).toWireString === """(:id 7 :touched-files ("/foo/abc" "/foo/def"))""")

      assert(toWF(null).toWireString === """nil""")
      assert(toWF(DebugVmSuccess()).toWireString === """(:status "success")""")
      assert(toWF(DebugVmError(303, "xxxx")).toWireString === """(:status "error" :error-code 303 :details "xxxx")""")

      // toWF(method: MethodBytecode)
      assert(toWF(MethodBytecode("className1", "methodName1",
        Some("MethodSig"), List(Op("op", "desc")), 7, 11)).toWireString === """(:class-name "className1" :name "methodName1" :signature "MethodSig" :bytecode (("op" "desc")))""")
    }
  }
}
