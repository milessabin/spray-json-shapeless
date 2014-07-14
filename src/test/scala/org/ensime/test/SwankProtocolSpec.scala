package org.ensime.test

import java.io.{ File, ByteArrayInputStream, ByteArrayOutputStream }

import akka.actor.TypedActor.MethodCall
import akka.actor.{ TypedProps, TypedActor, ActorSystem }
import akka.testkit.TestProbe
import org.ensime.server.RPCTarget
import org.ensime.util.SExp
import org.scalatest.{ BeforeAndAfterAll, FunSpec, ShouldMatchers }
import org.ensime.protocol.{ OutgoingMessageEvent, SExpConversion }
import org.ensime.protocol.SwankProtocol
import scala.reflect.io.ZipArchive
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.io.{ VirtualFile, PlainFile }
import scala.concurrent.duration._

class SwankProtocolSpec extends FunSpec with ShouldMatchers with BeforeAndAfterAll {

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
      val encodedMsg = SExp.read(msg)
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
    it("replies to connection info request") {
      withTestConfig { (protocol, rpcProbe, outputProbe) =>
        protocol.handleIncomingMessage(SExp.read("""(:swank-rpc (swank:connection-info) 42)"""))

        val expected = SExp.read("""(:return (:ok (:pid nil :implementation (:name "ENSIME-ReferenceServer") :version "0.8.9")) 42))""")
        outputProbe.expectMsg(1.seconds, OutgoingMessageEvent(expected))
        rpcProbe.expectNoMsg()
      }
    }

    it("processes remove-file request") {
      testRPCMethod("""(:swank-rpc (swank:remove-file "Analyzer.scala") 42)""",
        "rpcRemoveFile",
        "Analyzer.scala", 42)
    }

    it("can encode and decode sexp to wire") {
      val msg = SExp.read("""(:XXXX "abc") """)
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
      val got = s.toReadableString(debug = false)
      assert(got == expected, got + " != " + expected)
    }

    it("can convert a Position in a ZipArchive entry") {
      val a = ZipArchive.fromFile(new File("stuff.zip"))
      val f = new BatchSourceFile(new MockZipEntry("stuff", a), "ABCDEF")
      val p = f.position(2)
      val s = SExpConversion.posToSExp(p)
      val expected = """(:file "stuff" :archive "stuff.zip" :offset 2)"""
      val got = s.toReadableString(debug = false)
      assert(got == expected, got + " != " + expected)
    }

    it("can convert a RangePosition") {
      val f = new BatchSourceFile(new PlainFile("stuff"), "ABCDEF")
      val p = new RangePosition(f, 1, 2, 3)
      val s = SExpConversion.posToSExp(p)
      val expected = """(:file "stuff" :offset 2 :start 1 :end 3)"""
      val got = s.toReadableString(debug = false)
      assert(got == expected, got + " != " + expected)
    }

    it("can convert a RangePosition in a ZipArchive entry") {
      val a = ZipArchive.fromFile(new File("stuff.zip"))
      val f = new BatchSourceFile(new MockZipEntry("stuff", a), "ABCDEF")
      val p = new RangePosition(f, 1, 2, 3)
      val s = SExpConversion.posToSExp(p)
      val expected = """(:file "stuff" :archive "stuff.zip" :offset 2 :start 1 :end 3)"""
      val got = s.toReadableString(debug = false)
      assert(got == expected, got + " != " + expected)
    }
  }
}
