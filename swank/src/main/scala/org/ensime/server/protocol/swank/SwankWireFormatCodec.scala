package org.ensime.server.protocol.swank

import java.io.{ EOFException, OutputStream }

import org.ensime.sexp.Sexp
import org.ensime.sexp.SexpParser
import org.slf4j.Logger

/**
 * Factors out some of the S-Express marshalling.
 *
 * WireFormat needs to be completely internal to allow protocol
 * implementations to be independent of representation.
 */
trait SwankWireFormatCodec {
  type WireFormat = Sexp

  protected def log: Logger

  def writeMessage(value: WireFormat, out: OutputStream): Unit = {
    val dataString: String = value.compactPrint
    val data: Array[Byte] = dataString.getBytes("UTF-8")
    val header: Array[Byte] = "%06x".format(data.length).getBytes("UTF-8")

    val displayStr = if (dataString.length > 512)
      dataString.take(512) + "..."
    else
      dataString
    log.info("Writing: " + displayStr)

    out.write(header)
    out.write(data)
    out.flush()
  }

  private def fillArray(in: java.io.Reader, a: Array[Char]): Unit = {
    var n = 0
    val l = a.length
    var charsRead = 0
    while (n < l) {
      charsRead = in.read(a, n, l - n)
      if (charsRead == -1) {
        throw new EOFException("End of file reached in socket reader.")
      } else {
        n += charsRead
      }
    }
  }

  def readMessage(reader: java.io.InputStreamReader): WireFormat = {
    val headerBuf = new Array[Char](6)
    fillArray(reader, headerBuf)
    val msgLen = Integer.valueOf(new String(headerBuf), 16).intValue()
    if (msgLen == 0)
      throw new IllegalStateException("Empty message read from socket!")

    val buf: Array[Char] = new Array[Char](msgLen)
    fillArray(reader, buf)

    val msgStr = new String(buf)
    val displayStr = if (msgStr.length > 500)
      msgStr.take(500) + "..."
    else
      msgStr
    log.info("Received msg: " + displayStr)

    SexpParser.parse(msgStr)
  }

}
