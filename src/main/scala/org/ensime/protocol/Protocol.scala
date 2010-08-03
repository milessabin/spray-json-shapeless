package org.ensime.protocol

import java.io._

import scala.actors._ 

import org.ensime.model._
import org.ensime.util._
import org.ensime.config.{ProjectConfig, DebugConfig, ReplConfig}
import org.ensime.debug.{DebugUnit, DebugSourceLinePairs}
import org.ensime.server.{RPCTarget, QuickTypeCheckResultEvent, FullTypeCheckResultEvent}


case class IncomingMessageEvent(obj:Any)
case class OutgoingMessageEvent(obj:Any)


trait Protocol extends ProtocolConversions{ 

  def readMessage(reader:Reader):WireFormat

  def writeMessage(value:WireFormat, writer:Writer)

  def sendBackgroundMessage(msg:String)

  def handleIncomingMessage(msg:Any)

  protected def peer:Actor

  // TODO: Perhaps a channel would be more efficient?
  def setOutputActor(peer:Actor)

  def setRPCTarget(target:RPCTarget)

  def sendMessage(o:WireFormat){
    peer ! OutgoingMessageEvent(o)
  }

  def sendRPCAckOK(callId:Int)

  def sendRPCReturn(value:WireFormat, callId:Int)

  def sendRPCReturnError(value:String, callId:Int)

  def sendRPCError(msg:String, callId:Int)

  def sendProtocolError(packet:String, condition:String)

  def sendConnectionInfo(callId:Int)

  def sendCompilerReady()

  def sendFullTypeCheckResult(result:FullTypeCheckResultEvent)
  
  def sendQuickTypeCheckResult(result:QuickTypeCheckResultEvent)

}

trait ProtocolConversions{
  def toWF(config:ReplConfig):WireFormat
  def toWF(config:DebugConfig):WireFormat
  def toWF(unit:DebugUnit):WireFormat
  def toWF(value:Boolean):WireFormat
  def toWF(value:DebugSourceLinePairs):WireFormat
  def toWF(value:NoteList):WireFormat
  def toWF(values:Iterable[WireFormat]):WireFormat
  def toWF(value:SymbolInfoLight):WireFormat
  def toWF(value:SymbolInfo):WireFormat
  def toWF(value:NamedTypeMemberInfoLight):WireFormat
  def toWF(value:NamedTypeMemberInfo):WireFormat
  def toWF(value:EntityInfo):WireFormat
  def toWF(value:TypeInfo):WireFormat
  def toWF(value:PackageInfo):WireFormat
  def toWF(value:CallCompletionInfo):WireFormat
  def toWF(value:InterfaceInfo):WireFormat
  def toWF(value:TypeInspectInfo):WireFormat
}
