package org.ensime.server

import java.io.PrintStream
import java.io._
import java.net.{ ServerSocket, Socket, InetAddress }
import akka.actor.{ ActorRef, Actor, Props, ActorSystem }
import org.ensime.protocol._
import org.ensime.util.WireFormat
import org.ensime.config.Environment
import org.slf4j._
import scala.Console
import scala.util.Properties._
import org.slf4j.bridge.SLF4JBridgeHandler

/**
 * For when your upstream dependencies can't be trusted with
 * stdout and stderr.
 */
object ConsoleOutputWorkaround {
  def redirectScalaConsole(): Unit = {
    // workaround SI-8717
    ConsoleRedirect.setOut(OutLog)
    ConsoleRedirect.setErr(ErrLog)
  }

  private val blacklist = Set("sun.", "java.", "scala.Console", "scala.Predef")
  private abstract class StreamToLog extends OutputStream {
    val buffer = new StringBuilder

    override def write(b: Int): Unit = try {
      val c = b.toChar
      if (c == '\n') {
        val message = buffer.toString
        buffer.clear()

        // reasonably expensive, but not as bad as printing to the
        // screen (which is very slow and blocking)
        val breadcrumbs = Thread.currentThread.getStackTrace.
          toList.drop(2).map(_.getClassName).filterNot {
            c => blacklist.exists(c.startsWith)
          }
        val logName = breadcrumbs.headOption.getOrElse("UNKNOWN SOURCE")
        val log = LoggerFactory.getLogger(logName)

        doLog(log, message)
      } else buffer.append(c)
    } catch {
      case t: Throwable => // bad logging shouldn't kill the app
    }

    protected def doLog(log: Logger, message: String): Unit
  }

  private object ErrLog extends StreamToLog {
    def doLog(log: Logger, m: String) = log.warn(m)
  }

  private object OutLog extends StreamToLog {
    def doLog(log: Logger, m: String) = log.info(m)
  }
}

object Server {
  SLF4JBridgeHandler.removeHandlersForRootLogger()
  SLF4JBridgeHandler.install()
  ConsoleOutputWorkaround.redirectScalaConsole()

  val log = LoggerFactory.getLogger(classOf[Server])

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
      log.warn("WARNING: org.ensime.server.Server now takes properties instead of arguments")
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

  import Server.log

  def startServer: Unit = {
    require(!cacheDir.exists || cacheDir.isDirectory, cacheDir + " is not a valid cache directory")
    cacheDir.mkdirs()

    val actorSystem = ActorSystem.create()
    val listener = new ServerSocket(requestedPort, 0, InetAddress.getByName(host))
    val actualPort = listener.getLocalPort

    log.info("ENSIME Server on " + host + ":" + actualPort)
    log.info("cacheDir=" + cacheDir)
    log.info(Environment.info)

    writePort(cacheDir, actualPort)

    val protocol = new SwankProtocol
    val project = new Project(protocol, actorSystem)

    try {
      while (true) {
        try {
          val socket = listener.accept()
          log.info("Got connection, creating handler...")
          val handler = actorSystem.actorOf(Props(classOf[SocketHandler], socket, protocol, project))
        } catch {
          case e: IOException =>
            log.error("ENSIME Server: ", e)
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
      log.info("CREATING " + portfile)
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
