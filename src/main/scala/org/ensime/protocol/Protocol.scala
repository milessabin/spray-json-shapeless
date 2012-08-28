/**
*  Copyright (c) 2010, Aemon Cannon
*  All rights reserved.
*  
*  Redistribution and use in source and binary forms, with or without
*  modification, are permitted provided that the following conditions are met:
*      * Redistributions of source code must retain the above copyright
*        notice, this list of conditions and the following disclaimer.
*      * Redistributions in binary form must reproduce the above copyright
*        notice, this list of conditions and the following disclaimer in the
*        documentation and/or other materials provided with the distribution.
*      * Neither the name of ENSIME nor the
*        names of its contributors may be used to endorse or promote products
*        derived from this software without specific prior written permission.
*  
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
*  DISCLAIMED. IN NO EVENT SHALL Aemon Cannon BE LIABLE FOR ANY
*  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package org.ensime.protocol

import java.io._
import org.ensime.config.{ ReplConfig, ProjectConfig }
import org.ensime.indexer.MethodBytecode
import org.ensime.model._
import org.ensime.server._
import org.ensime.util._
import scala.actors._
import scala.tools.nsc.util.{ Position, RangePosition }

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

trait Protocol extends ProtocolConversions {

  /**
   * Read a message from the socket.
   *
   * @param  reader  The stream from which to read the message.
   * @return         The message, in the intermediate format.
   */
  def readMessage(reader: InputStream): WireFormat

  /**
   * Write a message to the socket.
   *
   * @param  value  The message to write.
   * @param  writer The stream to which to write the message.
   * @return        Void
   */
  def writeMessage(value: WireFormat, writer: OutputStream)

  /**
   * Send a message in wire format to the client. Message
   * will be sent to the outputPeer, and then written to the
   * output socket.
   *
   * @param  o  The message to send.
   * @return    Void
   */
  def sendMessage(o: WireFormat) {
    peer ! OutgoingMessageEvent(o)
  }

  /**
   * Handle a message from the client. Generally
   * messages encode RPC calls, and will be delegated
   * to the rpcTarget.
   *
   * @param  msg  The message we've received.
   * @return        Void
   */
  def handleIncomingMessage(msg: Any)


  /**
   * Designate an actor that should receive outgoing 
   * messages. 
   * TODO: Perhaps a channel would be more efficient?
   *
   * @param  peer  The Actor.
   * @return        Void
   */
  def setOutputActor(peer: Actor)
  protected def peer: Actor

  /**
   * Designate the target to which RPC handling
   * should be delegated.
   *
   * @param  target The RPCTarget instance.
   * @return        Void
   */
  def setRPCTarget(target: RPCTarget)

  /**
   * Send a simple RPC Return with a 'true' value.
   * Serves to acknowledge the RPC call when no 
   * other return value is required.
   *
   * @param  callId The id of the RPC call.
   * @return        Void
   */
  def sendRPCAckOK(callId: Int)

  /**
   * Send an RPC Return with the given value.
   *
   * @param  value  The value to return.
   * @param  callId The id of the RPC call.
   * @return        Void
   */
  def sendRPCReturn(value: WireFormat, callId: Int)

  /**
   * Send an event.
   *
   * @param  value  The event value.
   * @return        Void
   */
  def sendEvent(value: WireFormat)

  /**
   * Notify the client that the RPC call could not
   * be handled.
   *
   * @param  code  Integer code denoting error type.
   * @param  detail  A message describing the error.
   * @param  callId The id of the failed RPC call.
   * @return        Void
   */
  def sendRPCError(code: Int, detail: Option[String], callId: Int)

  /**
   * Notify the client that a message was received
   * that does not conform to the protocol.
   *
   * @param  code  Integer code denoting error type.
   * @param  detail  A message describing the problem.
   * @return        Void
   */
  def sendProtocolError(code: Int, detail: Option[String])


}

trait ProtocolConversions {
  def toWF(evt: SendBackgroundMessageEvent): WireFormat
  def toWF(evt: AnalyzerReadyEvent): WireFormat
  def toWF(evt: FullTypeCheckCompleteEvent): WireFormat
  def toWF(evt: IndexerReadyEvent): WireFormat
  def toWF(evt: NewNotesEvent): WireFormat
  def toWF(evt: ClearAllNotesEvent): WireFormat
  def toWF(evt: DebugEvent): WireFormat

  def toWF(obj: DebugLocation): WireFormat
  def toWF(obj: DebugValue): WireFormat
  def toWF(evt: DebugNullValue): WireFormat
  def toWF(evt: DebugPrimitiveValue): WireFormat
  def toWF(evt: DebugClassField): WireFormat
  def toWF(obj: DebugStringInstance): WireFormat
  def toWF(evt: DebugObjectInstance): WireFormat
  def toWF(evt: DebugArrayInstance): WireFormat
  def toWF(evt: DebugStackLocal): WireFormat
  def toWF(evt: DebugStackFrame): WireFormat
  def toWF(evt: DebugBacktrace): WireFormat

  def toWF(pos: SourcePosition): WireFormat
  def toWF(config: BreakpointList): WireFormat
  def toWF(config: ProjectConfig): WireFormat
  def toWF(config: ReplConfig): WireFormat
  def toWF(value: Boolean): WireFormat
  def toWF(value: String): WireFormat
  def toWF(value: Note): WireFormat
  def toWF(notelist: NoteList): WireFormat;
  def toWF(values: Iterable[WireFormat]): WireFormat
  def toWF(value: CompletionInfo): WireFormat
  def toWF(value: CompletionInfoList): WireFormat
  def toWF(value: PackageMemberInfoLight): WireFormat
  def toWF(value: SymbolInfo): WireFormat
  def toWF(value: NamedTypeMemberInfoLight): WireFormat
  def toWF(value: NamedTypeMemberInfo): WireFormat
  def toWF(value: EntityInfo): WireFormat
  def toWF(value: TypeInfo): WireFormat
  def toWF(value: PackageInfo): WireFormat
  def toWF(value: CallCompletionInfo): WireFormat
  def toWF(value: InterfaceInfo): WireFormat
  def toWF(value: TypeInspectInfo): WireFormat
  def toWF(value: SymbolSearchResults): WireFormat
  def toWF(value: ImportSuggestions): WireFormat
  def toWF(value: SymbolSearchResult): WireFormat
  def toWF(value: Position): WireFormat
  def toWF(value: RangePosition): WireFormat
  def toWF(value: FileRange): WireFormat
  def toWF(value: SymbolDesignations): WireFormat

  def toWF(value: RefactorFailure): WireFormat
  def toWF(value: RefactorEffect): WireFormat
  def toWF(value: RefactorResult): WireFormat
  def toWF(value: Undo): WireFormat
  def toWF(value: UndoResult): WireFormat
  def toWF(value: Null): WireFormat
  def toWF(vmStatus: DebugVmStatus): WireFormat
  def toWF(method: MethodBytecode): WireFormat
}
