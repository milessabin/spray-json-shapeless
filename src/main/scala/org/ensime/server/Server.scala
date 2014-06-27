/**
 *  Copyright (c) 2010, Aemon Cannon
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *      * Redistributions of source code must retain the above copyright
 *        notice, this list of conditions and the following disclaimer.
 *      * Redistributions in binary form must reproduce the above copyright
 *        notice, this list of conditions and the following disclaimer in the
 *        documentation and/or other materials provided with the distribution.
 *      * Neither the name of ENSIME nor the
 *        names of its contributors may be used to endorse or promote products
 *        derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED. IN NO EVENT SHALL Aemon Cannon BE LIABLE FOR ANY
 *  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 *  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 *  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 *  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.ensime.server

import java.io._
import java.net.{ ServerSocket, Socket, InetAddress }
import org.ensime.protocol._
import org.ensime.util.WireFormat
import org.ensime.config.Environment
import scala.actors._
import scala.actors.Actor._
import scala.util.Properties._

object Server {

  def main(args: Array[String]): Unit = {
    def setFallbackProp(name: String, fallback: String): Unit = {
      // this is nasty, use typesafe Config instead
      setProp(name, propOrElse(name, fallback))
    }
    setFallbackProp("ensime.cachedir", propOrNull("user.dir") + "/" + ".ensime_cache")
    setFallbackProp("actors.corePoolSize", "10")
    setFallbackProp("actors.maxPoolSize", "100")

    val (cacheDir, host, requestedPort) = if (args.length > 0) {
      // legacy interface
      System.err.println(
        "WARNING: org.ensime.server.Server now takes properties instead of arguments"
      )
      args match {
        case Array(a, b, c) => (
          new File(new File(a).getParentFile(), ".ensime_cache"), b, c.toInt)
        case Array(portfile) => (
          new File(new File(portfile).getParentFile(), ".ensime_cache"),
          "127.0.0.1", 0)
        case _ =>
          throw new IllegalArgumentException("org.ensime.server.Server invoked incorrectly")
      }
    } else (
      new File(propOrNull("ensime.cachedir")),
      propOrElse("ensime.hostname", "127.0.0.1"),
      propOrElse("ensime.requestport", "0").toInt
    )

    require(!cacheDir.exists || cacheDir.isDirectory, cacheDir + " is not a valid cache directory")
    cacheDir.mkdirs()

    val listener = new ServerSocket(requestedPort, 0, InetAddress.getByName(host))
    val actualPort = listener.getLocalPort

    println("ENSIME Server on " + host + ":" + actualPort)
    println("cacheDir=" + cacheDir)
    println(Environment.info)
    System.out.flush()

    writePort(cacheDir, actualPort)

    val protocol = SwankProtocol
    val project: Project = new Project(protocol)
    project.start()

    try {
      while (true) {
        try {
          val socket = listener.accept()
          println("Got connection, creating handler...")
          val handler = new SocketHandler(socket, protocol, project)
          handler.start()
        } catch {
          case e: IOException =>
            System.err.println("[ERROR] ENSIME Server: " + e)
        }
      }
    } finally {
      listener.close()
    }
  }

  private def writePort(cacheDir: File, port: Int) {
    val portfile = new File(cacheDir, "port")
    println("")
    if (!portfile.exists()) {
      println("CREATING " + portfile)
      portfile.createNewFile()
    } else if (portfile.length > 0)
      // LEGACY: older clients create an empty file
      throw new IOException(
        "An ENSIME server might be open already for this project. " +
          "If you are sure this is not the case, please delete " +
          portfile.getAbsolutePath + " and try again"
      )

    portfile.deleteOnExit()
    val out = new PrintWriter(portfile)
    try out.println(port)
    finally out.close()
  }
}

class SocketHandler(socket: Socket, protocol: Protocol, project: Project) extends Actor {
  protocol.setOutputActor(this)

  class SocketReader(socket: Socket, handler: SocketHandler) extends Actor {
    val in = new BufferedInputStream(socket.getInputStream());
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
            if (System.getProperty("ensime.explode.on.disconnect") != null) {
              println("Tick-tock, tick-tock, tick-tock... boom!")
              System.exit(-1)
            } else exit('error)
          }
      }
    }
  }

  val out = new BufferedOutputStream(socket.getOutputStream())

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
    val reader = new SocketReader(socket, this)
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
