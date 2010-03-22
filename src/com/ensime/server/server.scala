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
	case InMessage(sexp) => println("Project got message: " + sexp)
      }
    }
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
case class InMessage(sexp:SExp)
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
	  println(new String(headerBuf))
	  val msglen = Integer.valueOf(new String(headerBuf), 16).intValue()
	  if(msglen > 0){
	    val buf:Array[Char] = new Array[Char](msglen);
	    fillArray(buf)
	    println(new String(buf))
	    val sexp:SExp = SExp.read(new CharArrayReader(buf))
	    handler ! InMessage(sexp)
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
    out.write(sexp.toString())
    out.flush()
  }

  def act() {

    val reader:SocketReader = new SocketReader(socket, this)
    reader.start

    loop {  
      receive {
	case InMessage(sexp) => {
	  println("Relaying message to project...")
	  project ! InMessage(sexp)
	}
	case OutMessage(sexp) => {
	  println("Writing message to socket...")
	  write(sexp)	  
	}
      }
    }
  }

}  


