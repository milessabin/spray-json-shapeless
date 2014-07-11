package org.ensime.test.intg

import java.io.{ File => JFile }
import java.util.concurrent.TimeoutException

import akka.actor._
import akka.pattern.Patterns
import org.apache.commons.io.{ FileUtils => IOFileUtils }
import org.ensime.protocol.{ IncomingMessageEvent, OutgoingMessageEvent }
import org.ensime.server.Server
import org.ensime.util._
import org.scalatest.Assertions
import org.slf4j.LoggerFactory

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.io.{ File => SFile }

/**
 * Utility class to support integration level tests.
 */
object IntgUtil extends Assertions {

  class InteractorHelper(server: Server, actorSystem: ActorSystem) {
    private case class AsyncRequest(request: SExp)
    private case class RPCRequest(request: SExp)
    private case class RPCResponse(resp: SExp)

    val project = server.project
    private class InteractorHelperActor extends Actor with ActorLogging {

      var nextRPCid = 1
      var rpcExpectations: Map[Int, ActorRef] = Map.empty

      var asyncMsgs = Map[SExp, Int]()

      var outstandingAsyncs = Vector[(SExp, ActorRef)]()

      def processOutstandingRequests(): Unit = {
        outstandingAsyncs = outstandingAsyncs.filter {
          case (exp, sender) =>
            asyncMsgs.get(exp) match {
              case None =>
                true
              case Some(0) =>
                true
              case Some(n) =>
                asyncMsgs += (exp -> (n - 1))
                sender ! None
                false
            }
        }
      }

      override def receive: Receive = {
        case AsyncRequest(req) =>
          outstandingAsyncs = outstandingAsyncs :+ (req, sender())
          processOutstandingRequests()
        case RPCRequest(req) =>
          val rpcId = nextRPCid
          nextRPCid += 1
          val msg = s"""(:swank-rpc ${req.toWireString} $rpcId)"""
          project ! IncomingMessageEvent(SExp.read(msg))
          rpcExpectations += (rpcId -> sender())
        // this is the message from the server
        case OutgoingMessageEvent(obj) =>
          log.info("Received message from server: ")
          obj match {
            case receivedMsg @ SExpList(KeywordAtom(":return") :: res :: IntAtom(rpcId) :: Nil) =>
              log.info("Recognized as RPC response")
              rpcExpectations.get(rpcId) match {
                case Some(rpcSender) =>
                  rpcSender ! RPCResponse(res)
                  rpcExpectations = rpcExpectations - rpcId
                case None =>
                  log.error("got rpc response with no rpcSender")
              }
            case receivedMsg: SExp =>
              log.info("Non-rpc response - storing in async queue: " + receivedMsg)
              val newCount = asyncMsgs.getOrElse(receivedMsg, 0) + 1
              asyncMsgs = asyncMsgs + (receivedMsg -> newCount)
              processOutstandingRequests()

            case m => log.error("cannot handle received " + m)
          }
      }
    }

    def sendRPCExp(dur: FiniteDuration, msg: SExp): SExp = {
      val askRes = Patterns.ask(actor, RPCRequest(msg), dur)
      try {
        val rawResult = Await.result(askRes, Duration.Inf)
        rawResult match {
          case RPCResponse(s) =>
            s
          case r =>
            throw new IllegalStateException("Unknown type returned from rpc request: " + r)
        }
      } catch {
        case t: TimeoutException =>
          fail("Timed out waiting for rpc response to " + msg)
      }
    }

    def sendRPCString(dur: FiniteDuration, msg: String): String = {
      val sexpReq = SExp.read(msg)
      sendRPCExp(dur, sexpReq).toReadableString()
    }

    def expectRPC(dur: FiniteDuration, toSend: String, expected: String) {
      val res = sendRPCString(dur, toSend)
      if (res != expected) {
        println("Expected: " + expected)
        println("Received: " + res)
        fail("Expected and received do not match")
      }
    }

    def expectAsync(dur: FiniteDuration, expected: String) {

      val expectedSExp = SExp.read(expected)

      val askRes = Patterns.ask(actor, AsyncRequest(expectedSExp), dur)
      try {
        Await.result(askRes, Duration.Inf)
      } catch {
        case t: TimeoutException =>
          fail("Timed out waiting for async msg: " + expected)
      }

    }

    private val actor = actorSystem.actorOf(Props(new InteractorHelperActor()))
    server.protocol.setOutputActor(actor)
  }

  trait Interactor {
    def send(msg: String): Unit
    def receive(dur: FiniteDuration, msg: String): Unit
  }
  /**
   * Run an integration test based on the given project
   * @param projectSource The directory containing the test project (will not be modified)
   * @param projectName The ensime name of the project
   * @param f The test function to run
   */
  def withTestProject(projectSource: String, projectName: String)(f: (JFile, InteractorHelper) => Unit): Unit = {
    val log = LoggerFactory.getLogger("IntgTest_" + projectName)

    FileUtils.withTemporaryDirectory { projectBase =>
      log.info("Target dir = " + projectBase)
      log.info("Copying files")
      IOFileUtils.copyDirectory(new java.io.File("src/test/resources/intg/simple"), projectBase)
      log.info("Building ensime configuration")
      val buildProcess = scala.sys.process.Process(List("sbt", "compile", "ensime"), Some(projectBase))
      buildProcess.!
      log.info("Build done")
      val dotEnsimeFile = SFile(new JFile(projectBase, ".ensime")).lines().drop(3).mkString("\n") // slurp()

      val cacheDir = new JFile(projectBase, ".ensime_cache")

      val server = new Server(cacheDir, "localhost", 0)

      implicit val actorSystem = server.actorSystem

      val interactor = new InteractorHelper(server, actorSystem)
      interactor.expectRPC(1 seconds, "(swank:connection-info)",
        """(:ok (:pid nil :implementation (:name "ENSIME-ReferenceServer") :version "0.8.9"))""")

      // drop the last brace in the ensime file and add some extra confg
      val configStr = dotEnsimeFile.trim.dropRight(1) +
        s"""
         | :rootDir "$projectBase"
         | :source-jars-dir "$projectBase/.ensime_cache/dep-src/source-jars/"
         | :active-subproject "simple"
         |)
         """.stripMargin

      val initMsg = s"""(swank:init-project
                        | $configStr
                        | )""".stripMargin

      interactor.expectRPC(3 seconds, initMsg,
        s"""(:ok (:project-name "$projectName" :source-roots ("$projectBase/src/main/scala")))""")

      interactor.expectAsync(30 seconds, """(:background-message 105 "Initializing Analyzer. Please wait...")""")
      interactor.expectAsync(30 seconds, """(:compiler-ready)""")
      interactor.expectAsync(30 seconds, """(:full-typecheck-finished)""")
      interactor.expectAsync(30 seconds, """(:indexer-ready)""")

      f(projectBase, interactor)

      server.shutdown()

    }

  }

}
