package com.ensime.server

import java.io._
import java.net.{InetAddress,ServerSocket,Socket,SocketException}
import java.util.Random
import scala.actors._  
import scala.actors.Actor._ 
import com.ensime.util.SExp

object Server {
  def main(args: Array[String]): Unit = {
    try {
      // TODO use a real cmdline parser here
      val portfile = args(0)
      val project:Project = new Project()
      project.start

      // 0 will cause socket to bind to first available port
      val requestedPort = 0
      val listener = new ServerSocket(requestedPort)
      val actualPort = listener.getLocalPort
      println("Server listening on " + actualPort + "..")
      writePort(portfile, actualPort)
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
	System.err.println("Server listen failed: " + e)
	System.exit(-1)
      }
    }
  }


  private def writePort(filename:String, port:Int){
    try{
      val out = new OutputStreamWriter(new FileOutputStream(filename))
      out.write(port.toString)
      out.flush()
      System.out.println("Wrote port " + port + " to " + filename + ".")
    }
    catch {
      case e: IOException => 
      {
	System.err.println("Could not write port to " + filename + ". " + e)
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
      var charsRead = 0;
      while(charsRead > -1 && n < l){
	charsRead = in.read(a, n, l - n)
	n += charsRead
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
	    //TODO allocating a new array each time is inefficient!
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
    val data:String = sexp.toReadableString
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
	  project ! SwankInMessageEvent(sexp)
	}
	case SwankOutMessageEvent(sexp) => {
	  write(sexp)	  
	}
      }
    }
  }

}  


