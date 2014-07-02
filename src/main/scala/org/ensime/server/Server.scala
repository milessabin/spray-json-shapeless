package org.ensime.server

import java.io._
import java.net.{ ServerSocket, Socket, InetAddress }
import akka.actor.{ ActorRef, Actor, Props, ActorSystem }
import org.ensime.protocol._
import org.ensime.util.WireFormat
import org.ensime.config.Environment
import org.slf4j.LoggerFactory
import scala.util.Properties._

object Server {
  val logger = LoggerFactory.getLogger("Server")

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
      logger.warn("WARNING: org.ensime.server.Server now takes properties instead of arguments")
      args match {
        case Array(a, b, c) => (
          new File(new File(a).getParentFile, ".ensime_cache"), b, c.toInt)
        case Array(portfile) => (
          new File(new File(portfile).getParentFile, ".ensime_cache"), "127.0.0.1", 0)
        case _ =>
          throw new IllegalArgumentException("org.ensime.server.Server invoked incorrectly")
      }
    } else (
      new File(propOrNull("ensime.cachedir")),
      propOrElse("ensime.hostname", "127.0.0.1"),
      propOrElse("ensime.requestport", "0").toInt
    )

    val server = new Server(cacheDir, host, requestedPort)
    server.startServer
  }
}

class Server(cacheDir: File, host: String, requestedPort: Int) {

  import Server.logger

  def startServer: Unit = {
    require(!cacheDir.exists || cacheDir.isDirectory, cacheDir + " is not a valid cache directory")
    cacheDir.mkdirs()

    val actorSystem = ActorSystem.create()
    val listener = new ServerSocket(requestedPort, 0, InetAddress.getByName(host))
    val actualPort = listener.getLocalPort

    logger.info("ENSIME Server on " + host + ":" + actualPort)
    logger.info("cacheDir=" + cacheDir)
    logger.info(Environment.info)

    writePort(cacheDir, actualPort)

    val protocol = new SwankProtocol
    val project = new Project(protocol, actorSystem)

    try {
      while (true) {
        try {
          val socket = listener.accept()
          logger.info("Got connection, creating handler...")
          val handler = actorSystem.actorOf(Props(classOf[SocketHandler], socket, protocol, project))
        } catch {
          case e: IOException =>
            logger.error("ENSIME Server: ", e)
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
      logger.info("CREATING " + portfile)
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

case object SocketClosed

class SocketReader(socket: Socket, protocol: Protocol, handler: ActorRef) extends Thread {
  val in = new BufferedInputStream(socket.getInputStream)

  override def run() {
    try {
      while (true) {
        val msg: WireFormat = protocol.readMessage(in)
        handler ! IncomingMessageEvent(msg)
      }
    } catch {
      case e: IOException =>
        System.err.println("Error in socket reader: " + e)
        if (System.getProperty("ensime.explode.on.disconnect") != null) {
          println("Tick-tock, tick-tock, tick-tock... boom!")
          System.out.flush()
          System.exit(-1)
        } else {
          handler ! SocketClosed
        }
    }
  }
}

class SocketHandler(socket: Socket, protocol: Protocol, project: Project) extends Actor {
  protocol.setOutputActor(self)

  val reader = new SocketReader(socket, protocol, self)
  val out = new BufferedOutputStream(socket.getOutputStream)

  def write(value: WireFormat) {
    try {
      protocol.writeMessage(value, out)
    } catch {
      case e: IOException =>
        System.err.println("Write to client failed: " + e)
        context.stop(self)
    }
  }

  override def preStart() {
    reader.start()
  }

  override def receive = {
    case IncomingMessageEvent(value: WireFormat) =>
      project ! IncomingMessageEvent(value)
    case OutgoingMessageEvent(value: WireFormat) =>
      write(value)
    case SocketClosed =>
      System.err.println("Socket closed, stopping self")
      context.stop(self)
  }
}
