package org.ensime.test

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, File }
import java.util.concurrent.atomic.AtomicInteger

import akka.actor.TypedActor.MethodCall
import akka.actor.{ ActorSystem, TypedActor, TypedProps }
import akka.testkit.TestProbe
import org.ensime.model.SourceFileInfo
import org.ensime.protocol.{ SExpConversion, SwankProtocol }
import org.ensime.server.RPCTarget
import org.ensime.util.{ SExp, SExpParser, WireFormat }
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
      val t = mock[RPCTarget]
      val out = mock[MsgHandler]

      val prot = new SwankProtocol() {
        override def sendMessage(o: WireFormat) {
          out.send(o)
        }
      }
      prot.setRPCTarget(t)

      val nextId = new AtomicInteger(1)
      def test(msg: String)(expectation: (RPCTarget, MsgHandler, Int) => Unit): Unit = {

        val rpcId = nextId.getAndIncrement
        expectation(t, out, rpcId)

        prot.handleIncomingMessage(SExpParser.read(s"(:swank-rpc $msg $rpcId)"))
      }

      test("(swank:connection-info)") { (t, m, id) =>
        (t.rpcConnectionInfo _).expects(id)
      }

      test("(swank:shutdown-server)") { (t, m, id) =>
        (t.rpcShutdownServer _).expects(id)
      }

      test("(swank:repl-config)") { (t, m, id) =>
        (t.rpcReplConfig _).expects(id)
      }

      test("""(swank:remove-file "Analyzer.scala")""") { (t, m, id) =>
        (t.rpcRemoveFile _).expects("Analyzer.scala", id)
      }

      val analyzerFile = new File("Analyzer.scala")
      test("""(swank:typecheck-file "Analyzer.scala")""") { (t, m, id) =>
        (t.rpcTypecheckFiles _).expects(List(SourceFileInfo(analyzerFile)), id)
      }

      test("""(swank:typecheck-file "Analyzer.scala" "contents\\n\\ncontents")""") { (t, m, id) =>

        (t.rpcTypecheckFiles _).expects(List(SourceFileInfo(analyzerFile, Some("contents\\n\\ncontents"))), id)
      }

    }
  }
}
