package com.ensime.server

import scala.actors._
import com.ensime.util.SExp._
import com.ensime.util._

case class SwankInMessageEvent(sexp:SExp)
case class SwankOutMessageEvent(sexp:SExp)

trait SwankHandler { self: Project =>

  val PROTOCOL_VERSION:String = "0.0.1"
  val SERVER_NAME:String = "ENSIMEserver"
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


  protected def sendEmacsBackgroundMessage(msg:String){
    send(SExp(
	key(":background-message"),
	msg
      ))
  }

  protected def sendEmacsRexOkReturn(callId:Int){
    send(SExp(
	key(":return"),
	SExp( key(":ok"), true ),
	callId
      ))
  }

  protected def sendEmacsRexReturn(value:SExp, callId:Int){
    send(SExp(
	key(":return"),
	value,
	callId
      ))
  }

  protected def sendEmacsRexError(msg:String, callId:Int){
    send(SExp(
	key(":invalid-rpc"),
	callId,
	msg
      ))
  }

  protected def sendEmacsRexErrorBadArgs(name:String, form:SExp, callId:Int){
    send(SExp(
	key(":invalid-rpc"),
	callId,
	"Malformed " + name + " call: " + form
      ))
  }

  protected def sendReaderError(packet:SExp, condition:String){
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
  protected def getConnectionInfo = {
    SExp(
      key(":pid"), 'nil,
      key(":server-implementation"),
      SExp(
	key(":name"), SERVER_NAME
      ),
      key(":machine"), 'nil,
      key(":features"), 'nil,
      key(":version"), PROTOCOL_VERSION
    )
  }

}
