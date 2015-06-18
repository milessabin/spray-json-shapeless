package org.ensime.server

import java.io._
import java.net.{ InetAddress, ServerSocket, Socket }
import java.util.concurrent.atomic.AtomicBoolean

import akka.event.slf4j.SLF4JLogging
import akka.actor._
import akka.event.LoggingReceive

import com.google.common.base.Charsets
import com.google.common.io.Files
import org.ensime.Protocol
import org.ensime.api._
import org.ensime.config._
import org.ensime.core._
import org.ensime.server.protocol._
import org.ensime.server.protocol.swank.SwankProtocol
import org.ensime.sexp.Sexp
import org.slf4j._
import org.slf4j.bridge.SLF4JBridgeHandler

import scala.concurrent.Future
import scala.util._
import Properties._
import scala.util.control.NonFatal

import pimpathon.java.io._

case class EnsimeServerError(description: String)

object Server {
  SLF4JBridgeHandler.removeHandlersForRootLogger()
  SLF4JBridgeHandler.install()

  val log = LoggerFactory.getLogger(classOf[Server])

  def main(args: Array[String]): Unit = {
    val ensimeFileStr = propOrNone("ensime.config").getOrElse(
      throw new RuntimeException("ensime.config (the location of the .ensime file) must be set")
    )

    val ensimeFile = new File(ensimeFileStr)
    if (!ensimeFile.exists() || !ensimeFile.isFile)
      throw new RuntimeException(s".ensime file ($ensimeFile) not found")

    implicit val config = try {
      EnsimeConfigProtocol.parse(Files.toString(ensimeFile, Charsets.UTF_8))
    } catch {
      case e: Throwable =>
        log.error(s"There was a problem parsing $ensimeFile", e)
        throw e
    }

    new Server("127.0.0.1", 0).start()
  }

}

/**
 * The Legacy Socket handler is a bit nasty --- it was originally
 * written before there were any decent Scala libraries for IO so we
 * end up doing Socket loops in Threads.
 *
 * It's crying out to be rewritten with akka.io.
 */
class Server(
    host: String,
    requestedPort: Int
)(
    implicit
    config: EnsimeConfig
) extends SLF4JLogging {
  // the config file parsing will attempt to create directories that are expected
  require(config.cacheDir.isDirectory, s"${config.cacheDir} is not a valid cache directory")

  implicit private val system = ActorSystem("ENSIME")

  // visible for testing
  val listener = new ServerSocket(requestedPort, 0, InetAddress.getByName(host))

  private val hasShutdownFlag = new AtomicBoolean
  private var loop: Thread = _

  log.info("ENSIME Server on " + host + ":" + listener.getLocalPort)
  log.info(Environment.info)

  writePort(config.cacheDir, listener.getLocalPort)

  def start(): Unit = {
    loop = new Thread {
      override def run(): Unit = {
        try while (!hasShutdownFlag.get()) {
          try {
            val socket = listener.accept()
            system.actorOf(SocketHandler(socket))
          } catch {
            case e: Exception =>
              if (!hasShutdownFlag.get())
                log.error("ENSIME Server socket listener error: ", e)
          }
        } finally listener.close()
      }
    }
    loop.setName("ENSIME Connection Loop")
    loop.start()
  }

  // TODO: attach this to the appropriate KILL signal
  def shutdown(): Unit = {
    hasShutdownFlag.set(true)
    Try(loop.interrupt())

    log.info("Shutting down the ActorSystem")
    Try(system.shutdown())

    log.info("Closing Socket listener")
    Try(listener.close())

    log.info("Awaiting actor system termination")
    Try(system.awaitTermination())

    log.info("Shutdown complete")
  }

  private def writePort(cacheDir: File, port: Int): Unit = {
    val portfile = new File(cacheDir, "port")
    if (!portfile.exists()) {
      log.info("creating portfile " + portfile)
      portfile.createNewFile()
    } else throw new IOException(
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

/**
 * This actor is spawned when a client connects to a Socket.
 */
class SocketHandler(
    socket: Socket,
    implicit val config: EnsimeConfig
) extends Actor with ActorLogging {
  import context.system

  private val protocol: Protocol = new SwankProtocol

  private var project: ActorRef = _
  private var docs: ActorRef = _

  private var in: InputStream = _
  private var out: OutputStream = _
  private var loop: Thread = _

  override def preStart(): Unit = {
    project = context.actorOf(Project(self), "project")
    docs = context.actorOf(Props(new DocServer(config)), "docs")

    in = socket.getInputStream.buffered
    out = socket.getOutputStream.buffered

    loop = new Thread {
      override def run(): Unit = while (true) try {
        val envelope = protocol.read(in)
        context.actorOf(RequestHandler(envelope, project, self, docs), s"${envelope.callId}")
      } catch {
        case NonFatal(e) =>
          log.error(e, "Error in socket reader: ")
        // otherwise ignore the message
      }
    }
    loop.setName("ENSIME Protocol Loop")
    loop.start()
  }

  override def postStop(): Unit = {
    Try(socket.close())
    Try(in.close())
    Try(out.close())
    Try(loop.interrupt())
  }

  def receive = LoggingReceive {
    case outgoing: EnsimeEvent => protocol.write(outgoing, out)
    case outgoing: RpcError => protocol.write(outgoing, out)
    case outgoing: RpcResponse =>
      try {
        protocol.write(outgoing, out)
      } catch {
        case NonFatal(t) =>
          log.error(t, s"Problem serialising $outgoing")
          protocol.write(RpcError(outgoing.callId, "Server error"), out)
      }
  }
}
object SocketHandler {
  def apply(socket: Socket)(implicit config: EnsimeConfig): Props =
    Props(classOf[SocketHandler], socket, config)
}

/**
 * Spawned to listen to the server's response to an RpcRequest.
 */
class RequestHandler(
    envelope: RpcRequestEnvelope,
    project: ActorRef,
    server: ActorRef,
    docs: ActorRef
) extends Actor with ActorLogging {

  override def preStart(): Unit = envelope.req match {
    // multi-phase queries
    case DocUriAtPointReq(_, _) | DocUriForSymbolReq(_, _, _) =>
      project ! envelope.req
      context.become(resolveDocSig, discardOld = false)

    case req => project ! req
  }

  def resolveDocSig: Receive = LoggingReceive.withLabel("resolveDocSig") {
    case None =>
      self ! false
      context.unbecome()
    case Some(sig: DocSigPair) =>
      docs ! DocUriReq(sig)
      context.unbecome()
  }

  // we can put all manner of timeout / monitoring logic in here

  def receive = LoggingReceive.withLabel("receive") {
    case EnsimeServerError(msg) =>
      server forward RpcError(envelope.callId, msg)
      context stop self

    // legacy --- to deal with bad/Optional actor responses
    case Some(response) =>
      server forward RpcResponse(envelope.callId, response)
      context stop self
    case None =>
      server forward RpcResponse(envelope.callId, false)
      context stop self

    case response =>
      server forward RpcResponse(envelope.callId, response)
      context stop self
  }

}
object RequestHandler {
  def apply(
    env: RpcRequestEnvelope,
    project: ActorRef,
    server: ActorRef,
    docs: ActorRef
  ): Props = Props(classOf[RequestHandler], env, project, server, docs)
}
