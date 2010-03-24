package com.ensime.server

import scala.actors._

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
      case SExpList(head::body) => {
	head match {
	  case KeywordAtom(key) => {
	    key match {
	      case ":emacs-rex" => {
		if(body.size != 3){
		  sendReaderError(sexp, "Expected five elements in :emacs-rex.")
		}
		else{
		  val form:SExp = body(0)
		  val pack:SExp = body(1)
		  val callId:SExp = body(2)
		  handleEmacsRex(form, callId)
		}
	      }
	    }
	  }
	  case _ => {
	    sendReaderError(sexp, "Unknown protocol form.")
	  }
	}
      }
      case _ => {
	sendReaderError(sexp, "Unknown protocol form.")
      }
    }
  }

  protected def handleEmacsRex(name:String, form:SExp, callId:SExp)

  protected def handleEmacsRex(form:SExp, callId:SExp){
    form match{
      case SExpList(list) => {
	list.head match {
	  case SymbolAtom(name) => {
	    handleEmacsRex(name, form, callId)
	  }
	  case _ => {
	    sendEmacsRexError(
	      "Unknown :emacs-rex call. Expecting leading symbol. " + form,
	      callId)
	  }
	}	
      }
    }
  }

  protected def sendEmacsRexOkReturn(callId:SExp){
    send(
      SExpList(
	List(
	  KeywordAtom(":return"),
	  SExpList(List(
	      KeywordAtom(":ok"), 
	      TruthAtom()
	    )),
	  callId
	)))
  }

  protected def sendEmacsRexReturn(value:SExp, callId:SExp){
    send(
      SExpList(
	List(
	  KeywordAtom(":return"),
	  value,
	  callId
	)))
  }

  protected def sendEmacsRexError(msg:String, callId:SExp){
    send(
      SExpList(
	List(
	  KeywordAtom(":invalid-rpc"),
	  callId,
	  StringAtom(msg)
	)))
  }

  protected def sendReaderError(packet:SExp, condition:String){
    send(
      SExpList(
	List(
	  KeywordAtom(":reader-error"),
	  packet,
	  StringAtom(condition)
	)))
  }

}
