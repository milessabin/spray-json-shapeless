package org.ensime.test

import java.io.{ InputStreamReader, ByteArrayInputStream, ByteArrayOutputStream, File }
import java.util.concurrent.atomic.AtomicInteger

import akka.actor.TypedActor.MethodCall
import akka.actor.{ ActorSystem, TypedActor, TypedProps }
import akka.testkit.TestProbe
import org.ensime.model._
import org.ensime.protocol.{ RPCTarget, SwankProtocol }
import org.ensime.server._
import org.ensime.util._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{ BeforeAndAfterAll, FunSpec, ShouldMatchers }

import scala.reflect.internal.util.{ RangePosition, BatchSourceFile }
import scala.reflect.io.ZipArchive
import scala.tools.nsc.io.{ PlainFile, VirtualFile }

class SwankProtocolSpec extends FunSpec with ShouldMatchers with BeforeAndAfterAll with MockFactory {

  class MockZipEntry(entry: String, archive: ZipArchive) extends VirtualFile(entry, entry) {
    override def underlyingSource: Option[ZipArchive] = Some(archive)
  }

  def withTestConfig(f: (SwankProtocol, TestProbe, TestProbe) => Unit) {

    implicit val actorSystem = ActorSystem.create()
    try {
      val typedSystem = TypedActor(actorSystem)

      val rpcTargetProbe = TestProbe()
      val outputProbe = TestProbe()
      val rpcProxy = typedSystem.typedActorOf(TypedProps[RPCTarget], rpcTargetProbe.ref)

      val protocol = new SwankProtocol()
      protocol.setRPCTarget(rpcProxy)
      protocol.setOutputActor(outputProbe.ref)

      f(protocol, rpcTargetProbe, outputProbe)

    } finally {
      actorSystem.shutdown()
    }
  }

  def testRPCMethod(msg: String, methodName: String, args: Any*) {
    withTestConfig { (protocol, rpcProbe, outputProbe) =>
      val encodedMsg = SExpParser.read(msg)
      protocol.handleIncomingMessage(encodedMsg)

      val rpcCall = rpcProbe.expectMsgType[MethodCall]
      assert(rpcCall.method.getName == methodName)
      val actualParams = rpcCall.parameters.toList
      val expectedParams = args.toList
      assert(expectedParams == actualParams)

      outputProbe.expectNoMsg()
    }
  }

  describe("SwankProtocol") {
    it("processes remove-file request") {
      testRPCMethod("""(:swank-rpc (swank:remove-file "Analyzer.scala") 42)""",
        "rpcRemoveFile",
        "Analyzer.scala", 42)
    }

    it("can encode and decode sexp to wire") {
      val msg = SExpParser.read("""(:XXXX "abc") """)
      val protocol = new SwankProtocol()

      val os = new ByteArrayOutputStream()
      protocol.writeMessage(msg, os)

      val reader = new InputStreamReader(new ByteArrayInputStream(os.toByteArray))
      val incomingMsg = protocol.readMessage(reader).asInstanceOf[SExp]

      assert(msg == incomingMsg)
    }

    trait MsgHandler {
      def send(s: String)
    }

    val nextId = new AtomicInteger(1)
    def test(msg: String)(expectation: (RPCTarget, MsgHandler, Int) => Unit): Unit = {

      val t = mock[RPCTarget]
      val out = mock[MsgHandler]

      val prot = new SwankProtocol() {
        override def sendMessage(o: WireFormat) {
          out.send(o.toString)
        }
      }
      prot.setRPCTarget(t)

      val rpcId = nextId.getAndIncrement
      expectation(t, out, rpcId)

      val sexp = SExpParser.read(s"(:swank-rpc $msg $rpcId)")
      assert(sexp != NilAtom)

      prot.handleIncomingMessage(sexp)
    }

    it("should understand swank:peek-undo") {
      test("""(swank:peek-undo)""") { (t, m, id) =>
        (t.rpcPeekUndo _).expects(id)
      }
    }

    it("should understand swank:exec-undo") {
      test("""(swank:exec-undo 1)""") { (t, m, id) =>
        (t.rpcExecUndo _).expects(1, id)
      }
    }

    it("should understand connection-info request") {
      test("(swank:connection-info)") { (t, m, id) =>
        (t.rpcConnectionInfo _).expects(id)
      }
    }

    it("should understand swank:init-project") {
      val configFragment = """(:use-sbt t :compiler-args ("-Ywarn-dead-code" "-Ywarn-catches" "-Xstrict-warnings") :root-dir "/Users/aemon/projects/ensime/")"""
      test(s"""(swank:init-project $configFragment)""") { (t, m, id) =>
        (t.rcpInitProject _).expects(SExpParser.read(configFragment), id)
      }
    }

    it("should understand swank:repl-config") {
      test("(swank:repl-config)") { (t, m, id) =>
        (t.rpcReplConfig _).expects(id)
      }
    }

    it("should understand swank:remove-file") {
      test("""(swank:remove-file "Analyzer.scala")""") { (t, m, id) =>
        (t.rpcRemoveFile _).expects("Analyzer.scala", id)
      }
    }

    val analyzerFile = new File("Analyzer.scala")
    val fooFile = new File("Foo.scala")

    it("should understand swank:swank:typecheck-file") {
      test("""(swank:typecheck-file "Analyzer.scala")""") { (t, m, id) =>
        (t.rpcTypecheckFiles _).expects(List(SourceFileInfo(analyzerFile)), id)
      }
    }

    it("should understand swank:swank:typecheck-file with content") {
      test("""(swank:typecheck-file "Analyzer.scala" "contents\\n\\ncontents")""") { (t, m, id) =>
        (t.rpcTypecheckFiles _).expects(List(SourceFileInfo(analyzerFile, Some("contents\\n\\ncontents"))), id)
      }
    }

    it("should understand swank:typecheck-files") {
      test("""(swank:typecheck-files ("Analyzer.scala" "Foo.scala"))""") { (t, m, id) =>
        (t.rpcTypecheckFiles _).expects(List(SourceFileInfo(analyzerFile), SourceFileInfo(fooFile)), id)
      }
    }

    it("should understand swank:atch-source") {
      test("""(swank:patch-source "Analyzer.scala" (("+" 6461 "Inc") ("-" 7127 7128) ("*" 7200 7300 "Bob")))""") { (t, m, id) =>
        (t.rpcPatchSource _).expects("Analyzer.scala", List(PatchInsert(6461, "Inc"), PatchDelete(7127, 7128), PatchReplace(7200, 7300, "Bob")), id)
      }
    }

    it("should understand swank:typecheck-all") {
      test("""(swank:typecheck-all)""") { (t, m, id) =>
        (t.rpcTypecheckAll _).expects(id)
      }
    }

    it("should understand swank:format-source") {
      test("""(swank:format-source ("/ensime/src/Test.scala"))""") { (t, m, id) =>
        (t.rpcFormatFiles _).expects(List("/ensime/src/Test.scala"), id)
      }
    }

    it("should understand swank:public-symbol-search") {
      test("""(swank:public-symbol-search ("java" "io" "File") 50)""") { (t, m, id) =>
        (t.rpcPublicSymbolSearch _).expects(List("java", "io", "File"), 50, id)
      }
    }

    it("should understand swank:import-suggestions") {
      test("""(swank:import-suggestions "/ensime/src/main/scala/org/ensime/server/Analyzer.scala" 2300 ("Actor") 10)""") { (t, m, id) =>
        (t.rpcImportSuggestions _).expects("/ensime/src/main/scala/org/ensime/server/Analyzer.scala", 2300, List("Actor"), 10, id)
      }
    }

    it("should understand swank:completions") {
      test(
        """(swank:completions
          | "/ensime/src/main/scala/org/ensime/protocol/SwankProtocol.scala"
          | 22626 0 t t)""".stripMargin) { (t, m, id) =>
          (t.rpcCompletionsAtPoint _).expects("/ensime/src/main/scala/org/ensime/protocol/SwankProtocol.scala", 22626, 0, true, true, id)
        }
    }

    it("should understand swank:package-member-completion") {
      test("""(swank:package-member-completion "org.ensime.server" "Server")""") { (t, m, id) =>
        (t.rpcPackageMemberCompletion _).expects("org.ensime.server", "Server", id)
      }
    }

    it("should understand swank:call-completion") {
      test("""(swank:call-completion 1)""") { (t, m, id) =>
        (t.rpcCallCompletion _).expects(1, id)
      }
    }

    it("should understand swank:uses-of-symbol-at-point") {
      test("""(swank:uses-of-symbol-at-point "Test.scala" 11334)""") { (t, m, id) =>
        (t.rpcUsesOfSymAtPoint _).expects("Test.scala", 11334, id)
      }
    }

    it("should understand swank:member-by-name") {
      test("""(swank:member-by-name "org.example.A" "x" t)""") { (t, m, id) =>
        (t.rpcMemberByName _).expects("org.example.A", "x", true, id)
      }
    }

    it("should understand swank:type-by-id") {
      test("""(swank:type-by-id 1381)""") { (t, m, id) =>
        (t.rpcTypeById _).expects(1381, id)
      }
    }

    it("should understand swank:type-by-name") {
      test("""(swank:type-by-name "java.lang.String")""") { (t, m, id) =>
        (t.rpcTypeByName _).expects("java.lang.String", id)
      }
    }

    it("should understand swank:type-by-name-at-point at point") {
      test("""(swank:type-by-name-at-point "String" "SwankProtocol.scala" 31680)""") { (t, m, id) =>
        (t.rpcTypeByNameAtPoint _).expects("String", "SwankProtocol.scala", OffsetRange(31680), id)
      }
    }

    it("should understand swank:type-by-name-at-point at range") {
      test("""(swank:type-by-name-at-point "String" "SwankProtocol.scala" (31680 31691))""") { (t, m, id) =>
        (t.rpcTypeByNameAtPoint _).expects("String", "SwankProtocol.scala", OffsetRange(31680, 31691), id)
      }
    }

    it("should understand swank:type-at-point at point") {
      test("""(swank:type-at-point "SwankProtocol.scala" 31680)""") { (t, m, id) =>
        (t.rpcTypeAtPoint _).expects("SwankProtocol.scala", OffsetRange(31680), id)
      }
    }

    it("should understand swank:type-at-point at range") {
      test("""(swank:type-at-point "SwankProtocol.scala" (31680 31691))""") { (t, m, id) =>
        (t.rpcTypeAtPoint _).expects("SwankProtocol.scala", OffsetRange(31680, 31691), id)
      }
    }

    it("should understand swank:inspect-type-at-point at point") {
      test("""(swank:inspect-type-at-point "SwankProtocol.scala" 32736)""") { (t, m, id) =>
        (t.rpcInspectTypeAtPoint _).expects("SwankProtocol.scala", OffsetRange(32736), id)
      }
    }

    it("should understand swank:inspect-type-at-point at range") {
      test("""(swank:inspect-type-at-point "SwankProtocol.scala" (32736 32740))""") { (t, m, id) =>
        (t.rpcInspectTypeAtPoint _).expects("SwankProtocol.scala", OffsetRange(32736, 32740), id)
      }
    }

    it("should understand swank:inspect-type-by-id") {
      test("""(swank:inspect-type-by-id 232)""") { (t, m, id) =>
        (t.rpcInspectTypeById _).expects(232, id)
      }
    }

    it("should understand swank:inspect-type-by-name") {
      test("""(swank:inspect-type-by-name "abc.d")""") { (t, m, id) =>
        (t.rpcInspectTypeByName _).expects("abc.d", id)
      }
    }

    it("should understand symbol-at-point") {
      test("""(swank:symbol-at-point "SwankProtocol.scala" 36483)""") { (t, m, id) =>
        (t.rpcSymbolAtPoint _).expects("SwankProtocol.scala", 36483, id)
      }
    }

    it("should understand swank:inspect-package-by-path") {
      test("""(swank:inspect-package-by-path "org.ensime.util")""") { (t, m, id) =>
        (t.rpcInspectPackageByPath _).expects("org.ensime.util", id)
      }
    }

    it("should understand swank:prepare-refactor - inline local") {
      test("""(swank:prepare-refactor 5 inlineLocal (file "SwankProtocol.scala" start 100 end 200) t)""") { (t, m, id) =>
        (t.rpcPrepareRefactor _).expects(5, InlineLocalRefactorDesc("SwankProtocol.scala", 100, 200), true, id)
      }
    }

    it("should understand swank:prepare-refactor - rename") {
      test("""(swank:prepare-refactor 6 rename (file "SwankProtocol.scala" start 39504 end 39508 newName "dude") t)""") { (t, m, id) =>
        (t.rpcPrepareRefactor _).expects(6, RenameRefactorDesc("dude", "SwankProtocol.scala", 39504, 39508), true, id)
      }
    }

    it("should understand swank:prepare-refactor - extractMethod") {
      test("""(swank:prepare-refactor 6 extractMethod (methodName "foo" file "SwankProtocol.scala" start 39504 end 39508) t)""") { (t, m, id) =>
        (t.rpcPrepareRefactor _).expects(6, ExtractMethodRefactorDesc("foo", "SwankProtocol.scala", 39504, 39508), true, id)
      }
    }

    it("should understand swank:prepare-refactor - extractLocal") {
      test("""(swank:prepare-refactor 6 extractLocal (name "foo" file "SwankProtocol.scala" start 39504 end 39508) t)""") { (t, m, id) =>
        (t.rpcPrepareRefactor _).expects(6, ExtractLocalRefactorDesc("foo", "SwankProtocol.scala", 39504, 39508), true, id)
      }
    }

    it("should understand swank:prepare-refactor - organiseImports") {
      test("""(swank:prepare-refactor 6 organiseImports (file "SwankProtocol.scala") t)""") { (t, m, id) =>
        (t.rpcPrepareRefactor _).expects(6, OrganiseImportsRefactorDesc("SwankProtocol.scala"), true, id)
      }
    }

    it("should not fail on swank:prepareRefactor from bug 573") {
      test("""(swank:prepare-refactor 2 organizeImports (file "/home/fommil/Projects/ensime-server/src/main/scala/org/ensime/indexer/DatabaseService.scala") t)""") { (t, m, id) =>
        (t.rpcPrepareRefactor _).expects(2, OrganiseImportsRefactorDesc("/home/fommil/Projects/ensime-server/src/main/scala/org/ensime/indexer/DatabaseService.scala"), true, id)
      }
    }

    it("should understand swank:prepare-refactor - addImport") {
      test("""(swank:prepare-refactor 6 addImport (qualifiedName "com.bar.Foo" file "SwankProtocol.scala" start 39504 end 39508) t)""") { (t, m, id) =>
        (t.rpcPrepareRefactor _).expects(6, AddImportRefactorDesc("com.bar.Foo", "SwankProtocol.scala", 39504, 39508), true, id)
      }
    }

    it("should understand swank:exec-refactor") {
      test("""(swank:exec-refactor 7 rename)""") { (t, m, id) =>
        (t.rpcExecRefactor _).expects(7, Symbol("rename"), id)
      }
    }

    it("should understand swank:cancel-refactor") {
      test("""(swank:cancel-refactor 1)""") { (t, m, id) =>
        (t.rpcCancelRefactor _).expects(1, id)
      }
    }

    it("should understand swank:symbol-designations") {
      test("""(swank:symbol-designations "SwankProtocol.scala" 0 46857 (var val varField valField))""") { (t, m, id) =>
        (t.rpcSymbolDesignations _).expects("SwankProtocol.scala", 0, 46857,
          List(Symbol("var"), Symbol("val"), Symbol("varField"), Symbol("valField")), id)
      }
    }

    it("should understand swank:expand-selection") {
      test("""(swank:expand-selection "Model.scala" 4648 4721)""") { (t, m, id) =>
        (t.rpcExpandSelection _).expects("Model.scala", 4648, 4721, id)
      }
    }

    it("should understand swank:method-bytecode") {
      test("""(swank:method-bytecode "hello.scala" 12)""") { (t, m, id) =>
        (t.rpcMethodBytecode _).expects("hello.scala", 12, id)
      }
    }

    it("should understand swank:debug-active-vm") {
      test("""(swank:debug-active-vm)""") { (t, m, id) =>
        (t.rpcDebugActiveVM _).expects(id)
      }
    }

    it("should understand swank:debug-start") {
      test("""(swank:debug-start "org.hello.HelloWorld arg")""") { (t, m, id) =>
        (t.rpcDebugStartVM _).expects("org.hello.HelloWorld arg", id)
      }
    }

    it("should understand swank:debug-attach") {
      test("""(swank:debug-attach "localhost" "9000")""") { (t, m, id) =>
        (t.rpcDebugAttachVM _).expects("localhost", "9000", id)
      }
    }

    it("should understand swank:debug-stop") {
      test("""(swank:debug-stop)""") { (t, m, id) =>
        (t.rpcDebugStopVM _).expects(id)
      }
    }

    it("should understand swank:debug-set-break") {
      test("""(swank:debug-set-break "hello.scala" 12)""") { (t, m, id) =>
        (t.rpcDebugBreak _).expects("hello.scala", 12, id)
      }
    }

    it("should understand swank:debug-clear-break") {
      test("""(swank:debug-clear-break "hello.scala" 12)""") { (t, m, id) =>
        (t.rpcDebugClearBreak _).expects("hello.scala", 12, id)
      }
    }

    it("should understand swank:debug-clear-all-breaks") {
      test("""(swank:debug-clear-all-breaks)""") { (t, m, id) =>
        (t.rpcDebugClearAllBreaks _).expects(id)
      }
    }

    it("debug-list-breakpoints test") {
      test("""(swank:debug-list-breakpoints)""") { (t, m, id) =>
        (t.rpcDebugListBreaks _).expects(id)
      }
    }

    it("should understand swank:debug-run") {
      test("""(swank:debug-run)""") { (t, m, id) =>
        (t.rpcDebugRun _).expects(id)
      }
    }

    it("should understand swank:debug-continue") {
      test("""(swank:debug-continue "1")""") { (t, m, id) =>
        (t.rpcDebugContinue _).expects(1L, id)
      }
    }

    it("should understand swank:debug-step") {
      test("""(swank:debug-step "982398123")""") { (t, m, id) =>
        (t.rpcDebugStep _).expects(982398123L, id)
      }
    }

    it("should understand swank:debug-next") {
      test("""(swank:debug-next "982398123")""") { (t, m, id) =>
        (t.rpcDebugNext _).expects(982398123L, id)
      }
    }

    it("should understand swank:debug-step-out") {
      test("""(swank:debug-step-out "982398123")""") { (t, m, id) =>
        (t.rpcDebugStepOut _).expects(982398123L, id)
      }
    }

    it("should understand swank:debug-locate-name") {
      test("""(swank:debug-locate-name "7" "apple")""") { (t, m, id) =>
        (t.rpcDebugLocateName _).expects(7L, "apple", id)
      }
    }

    it("should understand swank:debug-value - array") {
      test("""(swank:debug-value (:type element :object-id "23" :index 2))""") { (t, m, id) =>
        (t.rpcDebugValue _).expects(DebugArrayElement(23L, 2), id)
      }
    }

    it("should understand swank:debug-value - object") {
      test("""(swank:debug-value (:type reference :object-id "23"))""") { (t, m, id) =>
        (t.rpcDebugValue _).expects(DebugObjectReference(23L), id)
      }
    }

    it("should understand swank:debug-value - object field") {
      test("""(swank:debug-value (:type field :object-id "23" :field "fred"))""") { (t, m, id) =>
        (t.rpcDebugValue _).expects(DebugObjectField(23L, "fred"), id)
      }
    }

    it("should understand swank:debug-value - stack slot") {
      test("""(swank:debug-value (:type slot :thread-id "23" :frame 7 :offset 25))""") { (t, m, id) =>
        (t.rpcDebugValue _).expects(DebugStackSlot(23L, 7, 25), id)
      }
    }

    it("should understand swank:debug-to-string - array element") {
      test("""(swank:debug-to-string "2" (:type element :object-id "23" :index 2))""") { (t, m, id) =>
        (t.rpcDebugToString _).expects(2L, DebugArrayElement(23L, 2), id)
      }
    }

    it("should understand swank:debug-set-value - array element") {
      test("""(swank:debug-set-value (:type element :object-id "23" :index 2) "1")""") { (t, m, id) =>
        (t.rpcDebugSetValue _).expects(DebugArrayElement(23L, 2), "1", id)
      }
    }

    it("should understand swank:debug-backtrace") {
      test("""(swank:debug-backtrace "23" 0 2)""") { (t, m, id) =>
        (t.rpcDebugBacktrace _).expects(23L, 0, 2, id)
      }
    }

    it("should understand swank:shutdown-server") {
      test("(swank:shutdown-server)") { (t, m, id) =>
        (t.rpcShutdownServer _).expects(id)
      }
    }

    it("should rejected an invalid range expression") {
      test("""(swank:type-by-name-at-point "String" "SwankProtocol.scala" (broken range expression))""") { (t, m, id) =>
        (m.send _).expects(s"""(:return (:abort 202 "Malformed swank:type-by-name-at-point call: (swank:type-by-name-at-point \\"String\\" \\"SwankProtocol.scala\\" (broken range expression))") $id)""")
      }
    }

    it("should rejected an invalid patch expression") {
      test("""(swank:patch-source "Analyzer.scala" (("+" 6461 "Inc") ("???" 7127 7128)))""") { (t, m, id) =>
        (m.send _).expects(s"""(:return (:abort 202 "Malformed swank:patch-source call: (swank:patch-source \\"Analyzer.scala\\" ((\\"+\\" 6461 \\"Inc\\") (\\"???\\" 7127 7128)))") $id)""")
      }
    }

    it("should rejected an invalid symbol list expression") {
      test("""(swank:symbol-designations "SwankProtocol.scala" 0 46857 (var "fred"))""") { (t, m, id) =>
        (m.send _).expects(s"""(:return (:abort 202 "Malformed swank:symbol-designations call: (swank:symbol-designations \\"SwankProtocol.scala\\" 0 46857 (var \\"fred\\"))") $id)""")
      }
    }

    it("should rejected an invalid debug location expression") {
      test("""(swank:debug-value (:type unknown :object-id "23" :index 2))""") { (t, m, id) =>
        (m.send _).expects(s"""(:return (:abort 202 "Malformed swank:debug-value call: (swank:debug-value (:type unknown :object-id \\"23\\" :index 2))") $id)""")
      }
    }

    it("should return error for rpc calls that throw an exception") {
      test("(swank:shutdown-server)") { (t, m, id) =>
        (t.rpcShutdownServer _).expects(id).throws(new RuntimeException("Bang"))
        (m.send _).expects(s"""(:return (:abort 201 \"Bang\") $id)""")
      }
    }

    it("should return error for unknown refactor") {
      test("""(swank:prepare-refactor 9 unknownRefactor (file "SwankProtocol.scala") t)""") { (t, m, id) =>
        (m.send _).expects(s"""(:return (:abort 202 "Malformed prepare-refactor - Incorrect arguments or unknown refactor type: unknownRefactor: (swank:prepare-refactor 9 unknownRefactor (file \\"SwankProtocol.scala\\") t)") $id)""")
      }
    }
  }
}
