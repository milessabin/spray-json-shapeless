package org.ensime.protocol

import java.io.OutputStream

import org.ensime.util.WireFormat

trait ProtocolWireFormatCodec {

  def readMessage(reader: java.io.InputStreamReader): WireFormat
  def writeMessage(value: WireFormat, out: OutputStream): Unit
}
