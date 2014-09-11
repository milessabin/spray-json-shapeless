package org.ensime.protocol

import java.io._

import akka.actor.ActorRef
import org.ensime.util._

case class IncomingMessageEvent(obj: Any)
case class OutgoingMessageEvent(obj: Any)

object ProtocolConst {

  val MsgCompilerUnexpectedError = 101
  val MsgInitializingAnalyzer = 102

  val MsgBuildingEntireProject = 103
  val MsgBuildComplete = 104
  val MsgMisc = 105

  val ErrExceptionInDebugger = 200
  val ErrExceptionInRPC = 201
  val ErrMalformedRPC = 202
  val ErrUnrecognizedForm = 203
  val ErrUnrecognizedRPC = 204
  val ErrExceptionInBuilder = 205

  val ErrPeekUndoFailed = 206
  val ErrExecUndoFailed = 207

  val ErrFormatFailed = 208

  val ErrAnalyzerNotReady = 209
  val ErrExceptionInAnalyzer = 210

  val ErrFileDoesNotExist = 211

  val ErrExceptionInIndexer = 212

}

trait Protocol {

  /**
   * Protocols must expose an appropriate class for converting messages to
   * the underlying wire format
   */
  val conversions: ProtocolConversions

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
  def sendMessage(o: WireFormat): Unit = {
    if (peer != null)
      peer ! OutgoingMessageEvent(o)
  }

  /**
   * Handle a message from the client. Generally
   * messages encode RPC calls, and will be delegated
   * to the rpcTarget.
   *
   * @param  msg  The message we've received.
   */
  def handleIncomingMessage(msg: Any): Unit

  /**
   * Designate an actor that should receive outgoing
   * messages.
   *
   * @param  peer  The Actor.
   */
  def setOutputActor(peer: ActorRef): Unit
  protected def peer: ActorRef

  /**
   * Designate the target to which RPC handling
   * should be delegated.
   *
   * @param  target The RPCTarget instance.
   * @return        Void
   */
  def setRPCTarget(target: RPCTarget): Unit

  /**
   * Send a simple RPC Return with a 'true' value.
   * Serves to acknowledge the RPC call when no
   * other return value is required.
   *
   * @param  callId The id of the RPC call.
   */
  def sendRPCAckOK(callId: Int): Unit

  /**
   * Send an RPC Return with the given value.
   *
   * @param  value  The value to return.
   * @param  callId The id of the RPC call.
   */
  def sendRPCReturn(value: WireFormat, callId: Int): Unit

  /**
   * Send an event.
   *
   * @param  event  The event value.
   */
  def sendEvent(event: SwankEvent): Unit

  /**
   * Notify the client that the RPC call could not
   * be handled.
   *
   * @param  code  Integer code denoting error type.
   * @param  detail  A message describing the error.
   * @param  callId The id of the failed RPC call.
   */
  def sendRPCError(code: Int, detail: String, callId: Int): Unit

  /**
   * Notify the client that a message was received
   * that does not conform to the protocol.
   *
   * @param  code  Integer code denoting error type.
   * @param  detail  A message describing the problem.
   */
  def sendProtocolError(code: Int, detail: String): Unit
}
