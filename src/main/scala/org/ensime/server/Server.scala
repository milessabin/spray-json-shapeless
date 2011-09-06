/**
*  Copyright (C) 2010 Aemon Cannon
*
*  This program is free software; you can redistribute it and/or
*  modify it under the terms of the GNU General Public License as
*  published by the Free Software Foundation; either version 2 of
*  the License, or (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public
*  License along with this program; if not, write to the Free
*  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
*  MA 02111-1307, USA.
*/

package org.ensime.server

import java.io._
import java.net.{ ServerSocket, Socket }
import org.ensime.protocol._
import org.ensime.util.WireFormat
import scala.actors._
import scala.actors.Actor._

object Server {

  def main(args: Array[String]): Unit = {
    System.setProperty("actors.corePoolSize", "8")
    System.setProperty("actors.maxPoolSize", "20")

    args match{
      case Array(portfile) => 
      {
	// TODO add an option to change the protocol
	val protocol: Protocol = SwankProtocol

	val project: Project = new Project(protocol)
	project.start()

	try {
          // 0 will cause socket to bind to first available port
          val requestedPort = 0
          val listener = new ServerSocket(requestedPort)
          val actualPort = listener.getLocalPort
          println("Server listening on " + actualPort + "..")
          writePort(portfile, actualPort)
          while (true) {
            try {
              val socket = listener.accept()
              println("Got connection, creating handler...")
              val handler = new SocketHandler(socket, protocol, project)
              handler.start()
            } catch {
              case e: IOException =>
              {
		System.err.println("Error in server listen loop: " + e)
              }
            }
          }
          listener.close()
	} catch {
          case e: IOException =>
          {
            System.err.println("Server listen failed: " + e)
            System.exit(-1)
          }
	}
      }
      case _ => {
	println("Usage: PROGRAM <portfile>")
	System.exit(0)
      }
    }
  }

  private def writePort(filename: String, port: Int) {
    val out = new OutputStreamWriter(new FileOutputStream(filename))
    try {
      out.write(port.toString)
      out.flush()
      System.out.println("Wrote port " + port + " to " + filename + ".")
    } catch {
      case e: IOException =>
      {
	System.err.println("Could not write port to " + filename + ". " + e)
	System.exit(-1)
      }
    }
    finally {
      out.close()
    }
  }

}

class SocketHandler(socket: Socket, protocol: Protocol, project: Project) extends Actor {
  protocol.setOutputActor(this)

  class SocketReader(socket: Socket, handler: SocketHandler) extends Actor {
    val in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
    def act() {
      var running = true
      try {
        while (running) {
          val msg: WireFormat = protocol.readMessage(in)
          handler ! IncomingMessageEvent(msg)
        }
      } catch {
        case e: IOException =>
        {
          System.err.println("Error in socket reader: " + e)
          exit('error)
        }
      }
    }
  }

  val out = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));

  def write(value: WireFormat) {
    try {
      protocol.writeMessage(value, out)
    } catch {
      case e: IOException =>
      {
        System.err.println("Write to client failed: " + e)
        exit('error)
      }
    }
  }

  def act() {
    val reader: SocketReader = new SocketReader(socket, this)
    this.link(reader)
    reader.start()
    loop {
      receive {
        case IncomingMessageEvent(value: WireFormat) => {
          project ! IncomingMessageEvent(value)
        }
        case OutgoingMessageEvent(value: WireFormat) => {
          write(value)
        }
        case Exit(_: SocketReader, reason) => exit(reason)
      }
    }
  }
}
