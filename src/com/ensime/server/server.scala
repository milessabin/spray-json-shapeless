package com.ensime.server

import java.io._
import java.net.{InetAddress,ServerSocket,Socket,SocketException}
import java.util.Random
import scala.actors._  
import scala.actors.Actor._ 



object Server {
  def main(args: Array[String]): Unit = {
    try {
      val project:Project = new Project()
      project.start
      val listener = new ServerSocket(9999);
      println("Server listening on 9999...")
      while (true){
	val socket = listener.accept()
	println("Got connection, creating handler...")
	val handler = new SocketHandler(socket, project)
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

class SocketHandler(socket:Socket, project:Project) extends Actor { 

  project.setSwankPeer(this)

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
	    handler ! SwankInMessageEvent(sexp)
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
    val data:String = sexp.toString()
    val header:String = String.format("%06x", int2Integer(data.length))
    val msg = header + data
    println("Writing: " + msg)
    out.write(msg)
    out.flush()
  }

  def act() {

    val reader:SocketReader = new SocketReader(socket, this)
    reader.start

    loop {  
      receive {
	case SwankInMessageEvent(sexp) => {
	  println("Relaying message to project...")
	  project ! SwankInMessageEvent(sexp)
	}
	case SwankOutMessageEvent(sexp) => {
	  println("Writing message to socket...")
	  write(sexp)	  
	}
      }
    }
  }

}  


