package org.ensime.server

import java.io._
import java.net.{ InetAddress, ServerSocket, Socket }
import java.util.concurrent.atomic.AtomicBoolean

import akka.actor._
import org.ensime.config._
import org.ensime.protocol._
import org.ensime.util._
import org.slf4j._
import org.slf4j.bridge.SLF4JBridgeHandler

import scala.io.Source
import scala.util.Properties
import scala.util.Properties._

object Server {
  SLF4JBridgeHandler.removeHandlersForRootLogger()
  SLF4JBridgeHandler.install()

  val log = LoggerFactory.getLogger(classOf[Server])

  def main(args: Array[String]): Unit = {
    val ensimeFileStr = propOrNone("ensime.config").getOrElse(
      throw new RuntimeException("ensime.config (the location of the .ensime file) must be set"))

    val ensimeFile = new File(ensimeFileStr)
    if (!ensimeFile.exists() || !ensimeFile.isFile)
      throw new RuntimeException(s".ensime file ($ensimeFile) not found")

    val config = readEnsimeConfig(ensimeFile)

    initialiseServer(config)
  }

  def initialiseServer(config: EnsimeConfig): Server = {
    val server = new Server(config, "127.0.0.1", 0)
    server.start()
    server
  }

  /**
   * ******************************************************************************
   * Read a new style config from the given files.
   * @param ensimeFile The base ensime file.
   * @param rootDir The project root directory
   * @param cacheDir The
   * @return
   */
  def readEnsimeConfig(ensimeFile: File): EnsimeConfig = {
    val configSrc = Source.fromFile(ensimeFile)
    try {
      val content = configSrc.getLines().filterNot(_.startsWith(";;")).mkString("\n")
      val parsed = SExpParser.read(content).asInstanceOf[SExpList]
      EnsimeConfig.parse(parsed)
    } finally {
      configSrc.close()
    }
  }
}

class Server(config: EnsimeConfig, host: String, requestedPort: Int) {

  import org.ensime.server.Server.log

  // the config file parsing will attempt to create directories that are expected
  require(config.cacheDir.isDirectory, config.cacheDir + " is not a valid cache directory")

  val actorSystem = ActorSystem.create()
  // TODO move this to only be started when we want to receive
  val listener = new ServerSocket(requestedPort, 0, InetAddress.getByName(host))
  val actualPort = listener.getLocalPort

  log.info("ENSIME Server on " + host + ":" + actualPort)
  log.info(Environment.info)

  writePort(config.cacheDir, actualPort)

  val protocol = new SwankProtocol
  val project = new Project(config, protocol, actorSystem)

  def start() {
    project.initProject()
    startSocketListener()
  }

  private val hasShutdownFlag = new AtomicBoolean(false)
  def startSocketListener(): Unit = {
    val t = new Thread(new Runnable() {
      def run() {
        try {
          while (!hasShutdownFlag.get()) {
            try {
              val socket = listener.accept()
              log.info("Got connection, creating handler...")
              actorSystem.actorOf(Props(classOf[SocketHandler], socket, protocol, project))
            } catch {
              case e: IOException =>
                if (!hasShutdownFlag.get())
                  log.error("ENSIME Server socket listener error: ", e)
            }
          }
        } finally {
          listener.close()
        }
      }
    })
    t.start()
  }

  def shutdown() {
    log.info("Shutting down server")
    hasShutdownFlag.set(true)
    listener.close()
    actorSystem.shutdown()
    log.info("Awaiting actor system shutdown")
    actorSystem.awaitTermination()
    log.info("Shutdown complete")
  }
  private def writePort(cacheDir: File, port: Int): Unit = {
    val portfile = new File(cacheDir, "port")
    if (!portfile.exists()) {
      log.info("Creating portfile " + portfile)
      log.info("creating portfile " + portfile)
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
  val log = LoggerFactory.getLogger(this.getClass)
  val in = new BufferedInputStream(socket.getInputStream)
  val reader = new InputStreamReader(in, "UTF-8")

  override def run() {
    try {
      while (true) {
        val msg: WireFormat = protocol.readMessage(reader)
        handler ! IncomingMessageEvent(msg)
      }
    } catch {
      case e: IOException =>
        log.error("Error in socket reader: ", e)
        Properties.envOrNone("ensime.explode.on.disconnect") match {
          case Some(_) =>
            log.warn("tick, tick, tick, tick... boom!")
            System.exit(-1)
          case None =>
            handler ! SocketClosed
        }
    }
  }
}

class SocketHandler(socket: Socket, protocol: Protocol, project: Project) extends Actor with ActorLogging {
  protocol.setOutputActor(self)

  val reader = new SocketReader(socket, protocol, self)
  val out = new BufferedOutputStream(socket.getOutputStream)

  def write(value: WireFormat) {
    try {
      protocol.writeMessage(value, out)
    } catch {
      case e: IOException =>
        log.error(e, "Write to client failed")
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
      log.error("Socket closed, stopping self")
      context.stop(self)
  }
}
