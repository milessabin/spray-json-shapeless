package org.ensime.server

import org.ensime.server.model._
import org.ensime.server._

import scala.actors._ 
import java.io._


case class IncomingMessageEvent(obj:Any)
case class OutgoingMessageEvent(obj:Any)

trait WireWritable{
  def writeOut(out:Writer):Unit
}

trait Protocol[A <: WireWritable]{ self: Project =>


  protected def read

  protected def sendBackgroundMessage(msg:String)

  protected def handleIncomingMessage(msg:A)

  protected def peer:Actor

  // TODO: use a channel here?
  def setOutputActor(peer:Actor)

  protected def send(o:A){
    peer ! OutgoingMessageEvent(o)
  }

  protected def handleRPCRequest(name:String, form:A, callId:Int)

  protected def sendRPCAckOK(callId:Int)

  protected def sendRPCReturn(value:A, callId:Int)

  protected def sendRPCError(msg:String, callId:Int)

  protected def sendProtocolError(packet:A, condition:String)

  protected def sendConnectionInfo(callId:Int)
  
}
