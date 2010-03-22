package com.ensime.server

import java.io._
import java.net.{InetAddress,ServerSocket,Socket,SocketException}
import java.util.Random
import scala.actors._  
import scala.actors.Actor._  


object Project extends Actor{

  def act() {
    println("Project is alive.")
    loop {  
      receive {
	case InMessage(replyTo:Actor, sexp) => {
	  sexp match{
	    case SExpList(list) => {
	      if(list.size > 0){
		handleMessageForm(replyTo, list)
	      }
	    }
	  }
	}
      }
    }
  }

  def getConnectionInfoSExp = {
    SExpList(List())
  }

  def handleMessageForm(replyTo:Actor, list:List[SExp]){
    val head:SExp = list.head
    head match {
      case KeywordAtom(key) => {
	key match {
	  case ":emacs-rex" => {
	    if(list.size != 5){
	      sendReaderError(replyTo, SExpList(list), "Expected five elements in :emacs-rex.")
	    }
	    else{
	      val form:SExp = list(1)
	      val pack:SExp = list(2)
	      val thread:SExp = list(3)
	      val callId:SExp = list(4)
	      handleEmacsRex(replyTo, form, callId)
	    }
	  }
	}
      }
    }
  }

  def handleEmacsRex(replyTo:Actor, form:SExp, callId:SExp){
    form match{
      case SExpList(list) => {
	list.head match {
	  case KeywordAtom(key) => {
	    key match{
	      case "swank:connection-info" => {
		sendEmacsRexReturn(
		  replyTo, 
		  SExpList(List(	
		      KeywordAtom(":ok"),
		      getConnectionInfoSExp
		    )),
		  callId)
	      }
	    }
	  }
	  case other => {
	    sendEmacsRexError(
	      replyTo, 
	      "Unknown :emacs-rex call: " + other, 
	      callId)
	  }
	}	
      }
    }
  }

  def sendEmacsRexReturn(replyTo:Actor, value:SExp, callId:SExp){
    replyTo ! OutMessage(
      SExpList(
	List(
	  KeywordAtom(":return"),
	  value,
	  callId
	)))
  }

  def sendEmacsRexError(replyTo:Actor, msg:String, callId:SExp){
    replyTo ! OutMessage(
      SExpList(
	List(
	  KeywordAtom(":invalid-rpc"),
	  callId,
	  StringAtom(msg)
	)))
  }

  def sendReaderError(replyTo:Actor, packet:SExp, condition:String){
    replyTo ! OutMessage(
      SExpList(
	List(
	  KeywordAtom(":reader-error"),
	  packet,
	  StringAtom(condition)
	)))
  }

}


object Server {
  def main(args: Array[String]): Unit = {
    try {
      Project.start
      val listener = new ServerSocket(9999);
      println("Server listening on 9999...")
      while (true){
	val socket = listener.accept()
	println("Got connection, creating handler...")
	val handler = new SocketHandler(socket, Project)
	handler.start
      }
      listener.close()
    }
    catch {
      case e: IOException => 
      {
	System.err.println("Could not listen on port: 9999.")
	System.exit(-1)
      }
    }
  }

}



import scala.util.parsing.input.StreamReader
import scala.util.parsing.input.CharArrayReader

case class NewSocket(socket:Socket)
case class InMessage(handler:Actor, sexp:SExp)
case class OutMessage(sexp:SExp)

class SocketHandler(socket:Socket, project:Actor) extends Actor { 

  class SocketReader(socket:Socket, handler:SocketHandler) extends Actor { 
    val in = new BufferedReader(new InputStreamReader(socket.getInputStream()));

    private def fillArray(a:Array[Char]){
      var n = 0
      var l = a.length
      while(n < l){
	n += in.read(a, n, l - n)
      }
    }

    def act() {
      val headerBuf = new Array[Char](6);
      var running = true
      try{
	while(running){
	  fillArray(headerBuf)
	  val msglen = Integer.valueOf(new String(headerBuf), 16).intValue()
	  if(msglen > 0){
	    val buf:Array[Char] = new Array[Char](msglen);
	    fillArray(buf)
	    val sexp:SExp = SExp.read(new CharArrayReader(buf))
	    handler ! InMessage(this, sexp)
	  }
	}
      }
      catch {
	case e: IOException => 
	{
	  System.err.println("Error while reading from client socket: " + e)
	  System.exit(-1)
	}
      }
    }
  }

  val out = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));

  def write(sexp:SExp) {

    gotta write the length here.. !!!!

    out.write(sexp.toString())
    out.flush()
  }

  def act() {

    val reader:SocketReader = new SocketReader(socket, this)
    reader.start

    loop {  
      receive {
	case InMessage(from, sexp) => {
	  println("Relaying message to project...")
	  project ! InMessage(this, sexp)
	}
	case OutMessage(sexp) => {
	  println("Writing message to socket...")
	  write(sexp)	  
	}
      }
    }
  }

}  


