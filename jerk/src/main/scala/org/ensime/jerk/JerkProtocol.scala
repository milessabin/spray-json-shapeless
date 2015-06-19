package org.ensime.jerk

import java.io._

import org.ensime.Protocol
import org.ensime.api._

import spray.json._

// stopgap until we enable WebSockets
class JerkProtocol extends Protocol {
  import JerkEndpoints.EnsimeEventFormat
  import JerkEndpoints.RpcRequestEnvelopeFormat
  import JerkEndpoints.RpcErrorFormat
  import JerkEndpoints.unhappyFamily

  override def read(input: InputStream): RpcRequestEnvelope =
    readJson(input).convertTo[RpcRequestEnvelope]

  override def write(resp: RpcResponse, output: OutputStream): Unit = {
    val json = unhappyFamily(resp)
    writeJson(json, output)
  }

  override def write(event: EnsimeEvent, output: OutputStream): Unit =
    writeJson(event.toJson, output)

  override def write(error: RpcError, output: OutputStream): Unit =
    writeJson(error.toJson, output)

  private def writeJson(value: JsValue, out: OutputStream): Unit = {
    val dataString: String = value.compactPrint
    val data: Array[Byte] = dataString.getBytes("UTF-8")
    val header: Array[Byte] = "%06x".format(data.length).getBytes("UTF-8")

    out.write(header)
    out.write(data)
    out.flush()
  }

  private def readJson(input: InputStream): JsValue = {
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

    JsonParser(new String(buf))
  }

}
