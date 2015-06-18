package org.ensime.server.protocol.swank

import java.io._

import akka.event.slf4j.SLF4JLogging

import org.ensime.sexp._
import org.ensime.Protocol
import org.ensime.api._

class SwankProtocol extends Protocol with SLF4JLogging {
  import SwankProtocolRequest.RpcRequestEnvelopeFormat
  import SwankProtocolResponse.unhappyFamily
  import SwankProtocolResponse.EnsimeEventFormat

  // can throw if the message is bad
  override def read(input: InputStream): RpcRequestEnvelope =
    readSexp(input).convertTo[RpcRequestEnvelope]

  override def write(resp: RpcResponse, output: OutputStream): Unit = {
    val wrapped = SexpList(
      SexpSymbol(":return"),
      SexpList(SexpSymbol(":ok"), unhappyFamily(resp.msg)),
      SexpNumber(resp.callId)
    )
    writeSexp(wrapped, output)
  }

  override def write(event: EnsimeEvent, output: OutputStream): Unit =
    writeSexp(event.toSexp, output)

  override def write(error: RpcError, output: OutputStream): Unit = {
    val wrapped = SexpList(
      SexpSymbol(":return"),
      SexpList(SexpSymbol(":abort"), SexpNumber(666), SexpString(error.detail)),
      SexpNumber(error.callId)
    ) // error code is deprecated
    writeSexp(wrapped, output)
  }

  private def writeSexp(value: Sexp, out: OutputStream): Unit = {
    val dataString: String = value.compactPrint
    val data: Array[Byte] = dataString.getBytes("UTF-8")
    val header: Array[Byte] = "%06x".format(data.length).getBytes("UTF-8")

    out.write(header)
    out.write(data)
    out.flush()
  }

  private def readSexp(input: InputStream): Sexp = {
    val reader = new InputStreamReader(input)

    def fillArray(a: Array[Char]): Unit = {
      var n = 0
      while (n < a.length) {
        val read = reader.read(a, n, a.length - n)
        if (read != -1) n += read
        else throw new EOFException("End of file reached in socket reader.")
      }
    }

    val headerBuf = new Array[Char](6)
    fillArray(headerBuf)
    val msgLen = Integer.valueOf(new String(headerBuf), 16).intValue()
    if (msgLen == 0)
      throw new IllegalStateException("Empty message read from socket!")

    val buf: Array[Char] = new Array[Char](msgLen)
    fillArray(buf)

    SexpParser.parse(new String(buf))
  }

}
