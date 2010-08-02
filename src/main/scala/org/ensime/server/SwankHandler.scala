package org.ensime.server

import scala.actors._
import org.ensime.util.SExp._
import org.ensime.util._


trait SwankHandler extends Protocol[SExp]{ self: Project =>

  val PROTOCOL_VERSION:String = "0.0.1"
  
  val SERVER_NAME:String = "ENSIMEserver"

  private var peer:Actor = null;

  override def setOutputActor(peer:Actor){ this.peer = peer }

  //protected implicit def toWireFormat(o:String):SExp = StringAtom(o)

  override protected def handleIncomingMessage(msg:IncomingMessageEvent){
    msg match{
      case IncomingMessageEvent(sexp:SExp) => {
	handleMessageForm(sexp)
      }
    }
  }

  override protected def sendBackgroundMessage(msg:String){
    send(SExp(
	key(":background-message"),
	msg
      ))
  }

  private def handleMessageForm(sexp:SExp){
    sexp match{
      case SExpList(KeywordAtom(":emacs-rex")::form::IntAtom(callId)::rest) => {
	handleEmacsRex(form, callId)
      }
      case _ => {
	sendProtocolError(sexp, "Unknown protocol form.")
      }
    }
  }

  private def handleEmacsRex(form:SExp, callId:Int){
    form match{
      case SExpList(SymbolAtom(name)::rest) => {
	handleRPCRequest(name, form, callId)
      }
      case _ => {
	sendRPCError(
	  "Unknown :emacs-rex call. Expecting leading symbol. " + form,
	  callId)
      }
    }	
  }

  override protected def sendRPCAckOK(callId:Int){
    send(SExp(
	key(":return"),
	SExp( key(":ok"), true ),
	callId
      ))
  }

  override protected def sendRPCReturn(value:SExp, callId:Int){
    send(SExp(
	key(":return"),
	value,
	callId
      ))
  }

  override protected def sendRPCError(msg:String, callId:Int){
    send(SExp(
	key(":invalid-rpc"),
	callId,
	msg
      ))
  }

  override protected def sendProtocolError(packet:SExp, condition:String){
    send(
      SExp(
	key(":reader-error"),
	packet,
	condition
      ))
  }

  /*
  * A sexp describing the server configuration, per the Swank standard.
  */
  override protected def sendConnectionInfo(callId:Int) = {
    val info = SExp(
      key(":pid"), 'nil,
      key(":server-implementation"),
      SExp(
	key(":name"), SERVER_NAME
      ),
      key(":machine"), 'nil,
      key(":features"), 'nil,
      key(":version"), PROTOCOL_VERSION
    )
    sendRPCReturn(SExp(key(":ok"), info), callId)
  }

}
