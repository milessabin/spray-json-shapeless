package org.ensime.server.protocol.swank

import java.io._
import org.scalatest._

import java.nio.charset.Charset

import org.ensime.api._

import pimpathon.file._

/**
 * Augments the `SwankFormatsSpec` with additional input/response
 * message pairs (but the assignation of response to input is provided
 * by a mock).
 *
 * This test is somewhat redundant and should be adapted to test the
 * synchronous `EnsimeApi`.
 */
class SwankProtocolSpec extends FlatSpec with Matchers {
  import SwankTestData._
  val swank = new SwankProtocol

  "SwankProtocol" should "read an RPC request" in {
    testRequest(
      """(:swank-rpc (swank:type-at-point "SwankProtocol.scala" 31680) 111)""",
      TypeAtPointReq(file("SwankProtocol.scala").canon, OffsetRange(31680))
    )
  }

  it should "write an RPC response" in {
    testResponse(
      RpcResponse(666, typeInfo),
      s"""(:return (:ok $typeInfoStr) 666)"""
    )
  }

  it should "write an RPC error" in {
    testResponse(
      RpcError(999, "xxxx"),
      """(:return (:abort 666 "xxxx") 999)"""
    )
  }

  it should "write an async message" in {
    testResponse(
      AnalyzerReadyEvent,
      """(:compiler-ready)"""
    )
  }

  // from pimpathon SNAPSHOT
  implicit class StringPimps(string: String) {
    def toByteArray: Array[Byte] = string.getBytes(Charset.forName("UTF-8"))
  }

  def testRequest(request: String, parseTo: RpcRequest): Unit = {
    val data = request.toByteArray
    val in = new ByteArrayInputStream("%06x".format(data.length).toByteArray ++ data)
    swank.read(in).req shouldBe parseTo
  }

  def testResponse(response: RpcResponse, marshallTo: String): Unit = {
    val out = new ByteArrayOutputStream
    swank.write(response, out)
    new String(out.toByteArray).drop(6) shouldBe marshallTo
  }

  def testResponse(error: RpcError, marshallTo: String): Unit = {
    val out = new ByteArrayOutputStream
    swank.write(error, out)
    new String(out.toByteArray).drop(6) shouldBe marshallTo
  }

  def testResponse(event: EnsimeEvent, marshallTo: String): Unit = {
    val out = new ByteArrayOutputStream
    swank.write(event, out)
    new String(out.toByteArray).drop(6) shouldBe marshallTo
  }

}
