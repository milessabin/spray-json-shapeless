package org.ensime.test.intg

import akka.event.slf4j.SLF4JLogging
import java.io.{ File => JFile }
import java.util.concurrent.TimeoutException

import akka.actor._
import akka.pattern.Patterns
import org.apache.commons.io.filefilter.TrueFileFilter
import org.apache.commons.io.{ FileUtils => IOFileUtils }
import org.ensime.config.EnsimeConfig
import org.ensime.protocol.{ IncomingMessageEvent, OutgoingMessageEvent }
import org.ensime.server.Server
import org.ensime.test.TestUtil
import org.ensime.util._
import org.scalatest.Assertions
import org.slf4j.LoggerFactory

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source
import scala.reflect.io.{ File => SFile, Path }

import RichFile._
import pimpathon.file._

/**
 * Utility class to support integration level tests.
 */
object IntgUtil extends Assertions with SLF4JLogging {

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
          project ! IncomingMessageEvent(SExpParser.read(msg))
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
      val sexpReq = SExpParser.read(msg)
      sendRPCExp(dur, sexpReq).toString
    }

    def expectRPC(dur: FiniteDuration, toSend: String, expected: String) {
      val res = sendRPCString(dur, toSend)
      if (res != expected) {
        fail("Expected and received do not match:\n" + expected + "\n != \n" + res)
      }
    }

    def expectAsync(dur: FiniteDuration, expected: String) {

      val expectedSExp = SExpParser.read(expected)

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

  private def listFiles(srcDir: String): List[SFile] = {
    import scala.collection.JavaConversions._
    val jFiles = IOFileUtils.listFiles(
      new JFile(srcDir), TrueFileFilter.INSTANCE, TrueFileFilter.INSTANCE
    )
    jFiles.toList.map(SFile(_))
  }

  private def copyFilesEnsuringUnixLines(projectSource: String, projectBase: java.io.File): Unit = {
    val srcFiles = listFiles(projectSource)
    for (srcFile <- srcFiles) {
      val relativeSrc = Path(projectSource).relativize(srcFile).toFile
      val destFile = Path(projectBase) / relativeSrc
      destFile.parent.createDirectory()
      val writer = destFile.bufferedWriter()
      val source = Source.fromFile(srcFile.path, "UTF-8")
      try {
        source.getLines().foreach(line => {
          writer.write(line)
          writer.write("\n")
        })
      } finally {
        source.close()
        writer.close()
      }
    }
  }

  /**
   * Run an integration test based on the given project
   * @param projectSource The directory containing the test project (will not be modified)
   * @param f The test function to run
   */
  def withTestProject(path: String)(f: (EnsimeConfig, InteractorHelper) => Unit): Unit = {

    withTempDirectory { base =>
      val projectBase = base.canon

      log.info("Target dir = " + projectBase)
      log.info("Copying files from " + path)

      // copies all the test classes, this is more than is needed
      val config = TestUtil.basicConfig(base, testClasses = true, jars = false)
      val sourceRoot = config.modules.values.head.sourceRoots.head

      copyFilesEnsuringUnixLines(path, sourceRoot)

      log.info("copied: " + projectBase.tree.toList)

      val server = Server.initialiseServer(config)

      implicit val actorSystem = server.actorSystem

      val interactor = new InteractorHelper(server, actorSystem)
      interactor.expectRPC(1 seconds, "(swank:connection-info)",
        """(:ok (:pid nil :implementation (:name "ENSIME-ReferenceServer") :version "0.8.9"))""")

      val initMsg = s"(swank:init-project ())"

      // we have to break it out because sourceroots also contains src.zip
      val initResp = interactor.sendRPCString(3 seconds, initMsg)
      // return is of the form (:ok ( :project-name ...)) so extract the data
      val initExp = SExpExplorer(SExpParser.read(initResp)).asList.get(1).asMap
      //assert(initExp.getString(":project-name") == projectName)

      assert(initExp.getStringList(":source-roots").toSet.contains(sourceRoot.getAbsolutePath))
      //      interactor.expectAsync(30 seconds, """(:background-message 105 "Initializing Analyzer. Please wait...")""")
      //      interactor.expectAsync(30 seconds, """(:full-typecheck-finished)""")
      interactor.expectAsync(240 seconds, """(:indexer-ready)""")
      interactor.expectAsync(60 seconds, """(:compiler-ready)""")

      f(config, interactor)

      server.shutdown()
    }
  }
}
