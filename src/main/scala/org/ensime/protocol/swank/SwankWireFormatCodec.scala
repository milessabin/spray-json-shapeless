package org.ensime.protocol.swank

import java.io.{ EOFException, OutputStream }

import org.ensime.protocol.ProtocolWireFormatCodec
import org.ensime.util.{ SExpParser, WireFormat }
import org.slf4j.Logger

import scala.util.parsing.input

trait SwankWireFormatCodec extends ProtocolWireFormatCodec {

  val log: Logger

  // Handle reading / writing of messages
  override def writeMessage(value: WireFormat, out: OutputStream): Unit = {
    val dataString: String = value.toWireString
    val data: Array[Byte] = dataString.getBytes("UTF-8")
    val header: Array[Byte] = "%06x".format(data.length).getBytes("UTF-8")

    val displayStr = if (dataString.length > 3000)
      dataString.take(3000) + "..."
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

  private val headerBuf = new Array[Char](6)

  override def readMessage(reader: java.io.InputStreamReader): WireFormat = {
    fillArray(reader, headerBuf)
    val msglen = Integer.valueOf(new String(headerBuf), 16).intValue()
    if (msglen > 0) {
      val buf: Array[Char] = new Array[Char](msglen)
      fillArray(reader, buf)
      SExpParser.read(new input.CharArrayReader(buf))
    } else {
      throw new IllegalStateException("Empty message read from socket!")
    }
  }

}
