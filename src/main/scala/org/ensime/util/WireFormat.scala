package org.ensime.util

trait WireFormat {
  def toWireString: String
  def withRpcReturn(callId: Int): WireFormat
}
