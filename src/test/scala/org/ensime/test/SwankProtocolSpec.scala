package org.ensime.test

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, File }
import java.util.concurrent.atomic.AtomicInteger

import akka.actor.TypedActor.MethodCall
import akka.actor.{ ActorSystem, TypedActor, TypedProps }
import akka.testkit.TestProbe
import org.ensime.model._
import org.ensime.protocol.{ SExpConversion, SwankProtocol }
import org.ensime.server.RPCTarget
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

      val is = new ByteArrayInputStream(os.toByteArray)
      val incomingMsg = protocol.readMessage(is).asInstanceOf[SExp]

      assert(msg == incomingMsg)
    }

    it("can convert a Position") {
      val f = new BatchSourceFile(new PlainFile("stuff"), "ABCDEF")
      val p = f.position(2)
      val s = SExpConversion.posToSExp(p)
      val expected = """(:file "stuff" :offset 2)"""
      val got = s.toString
      assert(got == expected, got + " != " + expected)
    }

    it("can convert a Position in a ZipArchive entry") {
      val a = ZipArchive.fromFile(new File("stuff.zip"))
      val f = new BatchSourceFile(new MockZipEntry("stuff", a), "ABCDEF")
      val p = f.position(2)
      val s = SExpConversion.posToSExp(p)
      val expected = """(:file "stuff" :archive "stuff.zip" :offset 2)"""
      val got = s.toString
      assert(got == expected, got + " != " + expected)
    }

    it("can convert a RangePosition") {
      val f = new BatchSourceFile(new PlainFile("stuff"), "ABCDEF")
      val p = new RangePosition(f, 1, 2, 3)
      val s = SExpConversion.posToSExp(p)
      val expected = """(:file "stuff" :offset 2 :start 1 :end 3)"""
      val got = s.toString
      assert(got == expected, got + " != " + expected)
    }

    it("can convert a RangePosition in a ZipArchive entry") {
      val a = ZipArchive.fromFile(new File("stuff.zip"))
      val f = new BatchSourceFile(new MockZipEntry("stuff", a), "ABCDEF")
      val p = new RangePosition(f, 1, 2, 3)
      val s = SExpConversion.posToSExp(p)
      val expected = """(:file "stuff" :archive "stuff.zip" :offset 2 :start 1 :end 3)"""
      val got = s.toString
      assert(got == expected, got + " != " + expected)
    }

    it("should handle conversions correctly") {

      trait MsgHandler {
        def send(o: WireFormat)
      }

      val nextId = new AtomicInteger(1)
      def test(msg: String)(expectation: (RPCTarget, MsgHandler, Int) => Unit): Unit = {

        val t = mock[RPCTarget]
        val out = mock[MsgHandler]

        val prot = new SwankProtocol() {
          override def sendMessage(o: WireFormat) {
            out.send(o)
          }
        }
        prot.setRPCTarget(t)

        val rpcId = nextId.getAndIncrement
        expectation(t, out, rpcId)

        val sexp = SExpParser.read(s"(:swank-rpc $msg $rpcId)")
        assert(sexp != NilAtom)

        prot.handleIncomingMessage(sexp)
      }

      test("(swank:connection-info)") { (t, m, id) =>
        (t.rpcConnectionInfo _).expects(id)
      }

      val configFragment = """(:use-sbt t :compiler-args ("-Ywarn-dead-code" "-Ywarn-catches" "-Xstrict-warnings") :root-dir "/Users/aemon/projects/ensime/")"""
      test(s"""(swank:init-project $configFragment)""") { (t, m, id) =>
        (t.rcpInitProject _).expects(SExpParser.read(configFragment), id)
      }

      test("""(swank:peek-undo)""") { (t, m, id) =>
        (t.rpcPeekUndo _).expects(id)
      }

      test("""(swank:exec-undo 1)""") { (t, m, id) =>
        (t.rpcExecUndo _).expects(1, id)
      }

      test("(swank:repl-config)") { (t, m, id) =>
        (t.rpcReplConfig _).expects(id)
      }

      test("""(swank:remove-file "Analyzer.scala")""") { (t, m, id) =>
        (t.rpcRemoveFile _).expects("Analyzer.scala", id)
      }

      val analyzerFile = new File("Analyzer.scala")
      val fooFile = new File("Foo.scala")

      test("""(swank:typecheck-file "Analyzer.scala")""") { (t, m, id) =>
        (t.rpcTypecheckFiles _).expects(List(SourceFileInfo(analyzerFile)), id)
      }

      test("""(swank:typecheck-file "Analyzer.scala" "contents\\n\\ncontents")""") { (t, m, id) =>
        (t.rpcTypecheckFiles _).expects(List(SourceFileInfo(analyzerFile, Some("contents\\n\\ncontents"))), id)
      }

      test("""(swank:typecheck-files ("Analyzer.scala" "Foo.scala"))""") { (t, m, id) =>
        (t.rpcTypecheckFiles _).expects(List(SourceFileInfo(analyzerFile), SourceFileInfo(fooFile)), id)
      }

      test("""(swank:patch-source "Analyzer.scala" (("+" 6461 "Inc") ("-" 7127 7128) ("*" 7200 7300 "Bob")))""") { (t, m, id) =>
        (t.rpcPatchSource _).expects("Analyzer.scala", List(PatchInsert(6461, "Inc"), PatchDelete(7127, 7128), PatchReplace(7200, 7300, "Bob")), id)
      }

      test("""(swank:typecheck-all)""") { (t, m, id) =>
        (t.rpcTypecheckAll _).expects(id)
      }

      test("""(swank:format-source ("/ensime/src/Test.scala"))""") { (t, m, id) =>
        (t.rpcFormatFiles _).expects(List("/ensime/src/Test.scala"), id)
      }

      test("""(swank:public-symbol-search ("java" "io" "File") 50)""") { (t, m, id) =>
        (t.rpcPublicSymbolSearch _).expects(List("java", "io", "File"), 50, id)
      }

      test("""(swank:import-suggestions "/ensime/src/main/scala/org/ensime/server/Analyzer.scala" 2300 ("Actor") 10)""") { (t, m, id) =>
        (t.rpcImportSuggestions _).expects("/ensime/src/main/scala/org/ensime/server/Analyzer.scala", 2300, List("Actor"), 10, id)
      }

      test("""(swank:completions
             | "/ensime/src/main/scala/org/ensime/protocol/SwankProtocol.scala"
             | 22626 0 t t)""".stripMargin) { (t, m, id) =>
        (t.rpcCompletionsAtPoint _).expects("/ensime/src/main/scala/org/ensime/protocol/SwankProtocol.scala", 22626, 0, true, true, id)
      }

      test("""(swank:package-member-completion "org.ensime.server" "Server")""") { (t, m, id) =>
        (t.rpcPackageMemberCompletion _).expects("org.ensime.server", "Server", id)
      }

      test("""(swank:call-completion 1)""") { (t, m, id) =>
        (t.rpcCallCompletion _).expects(1, id)
      }

      test("""(swank:uses-of-symbol-at-point "Test.scala" 11334)""") { (t, m, id) =>
        (t.rpcUsesOfSymAtPoint _).expects("Test.scala", 11334, id)
      }

      test("""(swank:type-by-id 1381)""") { (t, m, id) =>
        (t.rpcTypeById _).expects(1381, id)
      }

      test("""(swank:type-by-id 1381)""") { (t, m, id) =>
        (t.rpcTypeById _).expects(1381, id)
      }

      test("""(swank:type-by-name "java.lang.String")""") { (t, m, id) =>
        (t.rpcTypeByName _).expects("java.lang.String", id)
      }

      test("""(swank:type-by-name-at-point "String" "SwankProtocol.scala" 31680)""") { (t, m, id) =>
        (t.rpcTypeByNameAtPoint _).expects("String", "SwankProtocol.scala", OffsetRange(31680), id)
      }

      test("""(swank:type-by-name-at-point "String" "SwankProtocol.scala" (31680 31691))""") { (t, m, id) =>
        (t.rpcTypeByNameAtPoint _).expects("String", "SwankProtocol.scala", OffsetRange(31680, 31691), id)
      }

      test("""(swank:type-at-point "SwankProtocol.scala" 31680)""") { (t, m, id) =>
        (t.rpcTypeAtPoint _).expects("SwankProtocol.scala", OffsetRange(31680), id)
      }

      test("""(swank:type-at-point "SwankProtocol.scala" (31680 31691))""") { (t, m, id) =>
        (t.rpcTypeAtPoint _).expects("SwankProtocol.scala", OffsetRange(31680, 31691), id)
      }

      test("""(swank:inspect-type-at-point "SwankProtocol.scala" 32736)""") { (t, m, id) =>
        (t.rpcInspectTypeAtPoint _).expects("SwankProtocol.scala", OffsetRange(32736), id)
      }

      test("""(swank:inspect-type-at-point "SwankProtocol.scala" (32736 32740))""") { (t, m, id) =>
        (t.rpcInspectTypeAtPoint _).expects("SwankProtocol.scala", OffsetRange(32736, 32740), id)
      }

      test("""(swank:inspect-type-by-id 232)""") { (t, m, id) =>
        (t.rpcInspectTypeById _).expects(232, id)
      }

      test("""(swank:symbol-at-point "SwankProtocol.scala" 36483)""") { (t, m, id) =>
        (t.rpcSymbolAtPoint _).expects("SwankProtocol.scala", 36483, id)
      }

      test("""(swank:inspect-package-by-path "org.ensime.util")""") { (t, m, id) =>
        (t.rpcInspectPackageByPath _).expects("org.ensime.util", id)
      }

      test("""(swank:prepare-refactor 6 rename (file "SwankProtocol.scala" start 39504 end 39508 newName "dude") t)""") { (t, m, id) =>
        val request: Map[Symbol, Any] =
          Map(
            Symbol("file") -> "SwankProtocol.scala",
            Symbol("start") -> 39504,
            Symbol("end") -> 39508,
            Symbol("newName") -> "dude"
          )
        (t.rpcPrepareRefactor _).expects(6, Symbol("rename"), request, true, id)
      }

      test("""(swank:exec-refactor 7 rename)""") { (t, m, id) =>
        (t.rpcExecRefactor _).expects(7, Symbol("rename"), id)
      }

      test("""(swank:cancel-refactor 1)""") { (t, m, id) =>
        (t.rpcCancelRefactor _).expects(1, id)
      }

      test("""(swank:symbol-designations "SwankProtocol.scala" 0 46857 (var val varField valField))""") { (t, m, id) =>
        (t.rpcSymbolDesignations _).expects("SwankProtocol.scala", 0, 46857,
          List(Symbol("var"), Symbol("val"), Symbol("varField"), Symbol("valField")), id)
      }

      test("""(swank:expand-selection "Model.scala" 4648 4721)""") { (t, m, id) =>
        (t.rpcExpandSelection _).expects("Model.scala", 4648, 4721, id)
      }

      test("""(swank:method-bytecode "hello.scala" 12)""") { (t, m, id) =>
        (t.rpcMethodBytecode _).expects("hello.scala", 12, id)
      }

      test("""(swank:debug-active-vm)""") { (t, m, id) =>
        (t.rpcDebugActiveVM _).expects(id)
      }

      test("""(swank:debug-start "org.hello.HelloWorld arg")""") { (t, m, id) =>
        (t.rpcDebugStartVM _).expects("org.hello.HelloWorld arg", id)
      }

      test("""(swank:debug-attach "localhost" "9000")""") { (t, m, id) =>
        (t.rpcDebugAttachVM _).expects("localhost", "9000", id)
      }

      test("""(swank:debug-stop)""") { (t, m, id) =>
        (t.rpcDebugStopVM _).expects(id)
      }

      test("""(swank:debug-set-break "hello.scala" 12)""") { (t, m, id) =>
        (t.rpcDebugBreak _).expects("hello.scala", 12, id)
      }

      test("""(swank:debug-clear-break "hello.scala" 12)""") { (t, m, id) =>
        (t.rpcDebugClearBreak _).expects("hello.scala", 12, id)
      }

      test("""(swank:debug-clear-all-breaks)""") { (t, m, id) =>
        (t.rpcDebugClearAllBreaks _).expects(id)
      }

      test("""(swank:debug-list-breakpoints)""") { (t, m, id) =>
        (t.rpcDebugListBreaks _).expects(id)
      }

      test("""(swank:debug-run)""") { (t, m, id) =>
        (t.rpcDebugRun _).expects(id)
      }

      test("""(swank:debug-continue "1")""") { (t, m, id) =>
        (t.rpcDebugContinue _).expects(1L, id)
      }

      test("""(swank:debug-step "982398123")""") { (t, m, id) =>
        (t.rpcDebugStep _).expects(982398123L, id)
      }

      test("""(swank:debug-next "982398123")""") { (t, m, id) =>
        (t.rpcDebugNext _).expects(982398123L, id)
      }

      test("""(swank:debug-step-out "982398123")""") { (t, m, id) =>
        (t.rpcDebugStepOut _).expects(982398123L, id)
      }

      test("""(swank:debug-locate-name "7" "apple")""") { (t, m, id) =>
        (t.rpcDebugLocateName _).expects(7L, "apple", id)
      }

      test("""(swank:debug-value (:type element :object-id "23" :index 2))""") { (t, m, id) =>
        (t.rpcDebugValue _).expects(DebugArrayElement(23L, 2), id)
      }

      test("""(swank:debug-value (:type reference :object-id "23"))""") { (t, m, id) =>
        (t.rpcDebugValue _).expects(DebugObjectReference(23L), id)
      }

      test("""(swank:debug-value (:type field :object-id "23" :field "fred"))""") { (t, m, id) =>
        (t.rpcDebugValue _).expects(DebugObjectField(23L, "fred"), id)
      }

      test("""(swank:debug-value (:type slot :thread-id "23" :frame 7 :offset 25))""") { (t, m, id) =>
        (t.rpcDebugValue _).expects(DebugStackSlot(23L, 7, 25), id)
      }

      test("""(swank:debug-to-string "2" (:type element :object-id "23" :index 2))""") { (t, m, id) =>
        (t.rpcDebugToString _).expects(2L, DebugArrayElement(23L, 2), id)
      }

      test("""(swank:debug-set-value (:type element :object-id "23" :index 2) "1")""") { (t, m, id) =>
        (t.rpcDebugSetValue _).expects(DebugArrayElement(23L, 2), "1", id)
      }

      test("""(swank:debug-backtrace "23" 0 2)""") { (t, m, id) =>
        (t.rpcDebugBacktrace _).expects(23L, 0, 2, id)
      }

      test("(swank:shutdown-server)") { (t, m, id) =>
        (t.rpcShutdownServer _).expects(id)
      }
    }
  }
}
