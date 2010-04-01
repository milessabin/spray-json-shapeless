package com.ensime.server

import scala.actors._
import com.ensime.server.SExp._

case class SwankInMessageEvent(sexp:SExp)
case class SwankOutMessageEvent(sexp:SExp)

trait SwankHandler { self: Project =>

  val PROTOCOL_VERSION:String = "0.0.1"
  val SERVER_NAME:String = "server_prototype"
  private var peer:Actor = null;

  def setSwankPeer(peer:Actor){
    this.peer = peer;
  }

  protected def send(sexp:SExp){
    peer ! SwankOutMessageEvent(sexp)
  }

  protected def handleIncomingSwankMessage(msg:SwankInMessageEvent){
    msg match{
      case SwankInMessageEvent(sexp:SExp) => {
	handleMessageForm(sexp)
      }
    }
  }


  protected def handleMessageForm(sexp:SExp){
    sexp match{
      case SExpList(KeywordAtom(":emacs-rex")::form::IntAtom(callId)::rest) => {
	handleEmacsRex(form, callId)
      }
      case _ => {
	sendReaderError(sexp, "Unknown protocol form.")
      }
    }
  }


  protected def handleEmacsRex(form:SExp, callId:Int){
    form match{
      case SExpList(SymbolAtom(name)::rest) => {
	handleEmacsRex(name, form, callId)
      }
      case _ => {
	sendEmacsRexError(
	  "Unknown :emacs-rex call. Expecting leading symbol. " + form,
	  callId)
      }
    }	
  }

  protected def handleEmacsRex(name:String, form:SExp, callId:Int)


  protected def sendEmacsRexOkReturn(callId:Int){
    send(SExp(
	":return",
	SExp( ":ok", true ),
	callId
      ))
  }

  protected def sendEmacsRexReturn(value:SExp, callId:Int){
    send(SExp(
	":return",
	value,
	callId
      ))
  }

  protected def sendEmacsRexError(msg:String, callId:Int){
    send(SExp(
	":invalid-rpc",
	callId,
	msg
      ))
  }

  protected def sendReaderError(packet:SExp, condition:String){
    send(
      SExp(
	":reader-error",
	packet,
	condition
      ))
  }

}
