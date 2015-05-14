package org.ensime.server.protocol

import java.io._

import org.ensime.api._

object ProtocolConst {

  val MsgCompilerUnexpectedError = 101
  val MsgMisc = 105

  val ErrExceptionInDebugger = 200
  val ErrExceptionInRPC = 201
  val ErrMalformedRPC = 202
  val ErrUnrecognizedForm = 203

  val ErrPeekUndoFailed = 206
  val ErrExecUndoFailed = 207

  val ErrFormatFailed = 208

  val ErrAnalyzerNotReady = 209
  val ErrExceptionInAnalyzer = 210

  val ErrFileDoesNotExist = 211

  val ErrExceptionInIndexer = 212
}

/**
 * TODO: WireFormat should be replaced with the domain objects, and
 * the list of methods reduced, to allow this to be replaced with
 * alternative wire format representations (e.g. S-Exp / JSON / XML).
 */
trait Protocol[WireFormat] {
  /**
   * Read a message from the socket.
   *
   * @param  reader  The stream from which to read the message.
   * @return         The message, in the intermediate format.
   */
  def readMessage(reader: InputStreamReader): WireFormat

  /**
   * Write a message to the socket.
   *
   * @param  value  The message to write.
   * @param  writer The stream to which to write the message.
   */
  def writeMessage(value: WireFormat, writer: OutputStream): Unit

  /**
   * Send a message in wire format to the client. Message
   * will be sent to the outputPeer, and then written to the
   * output socket.
   *
   * @param  o  The message to send.
   */
  def sendMessage(o: WireFormat): Unit

  /**
   * Handle a message from the client. Generally
   * messages encode RPC calls, and will be delegated
   * to the rpcTarget.
   *
   * @param  msg  The message we've received.
   */
  def handleIncomingMessage(msg: WireFormat): Unit

  /**
   * Notify the client that a message was received
   * that does not conform to the protocol.
   *
   * @param  code  Integer code denoting error type.
   * @param  detail  A message describing the problem.
   */
  def sendProtocolError(code: Int, detail: String): Unit
}
