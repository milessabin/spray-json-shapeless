package org.ensime.test.intg

import akka.event.slf4j.SLF4JLogging
import java.io.{ File => JFile }
import java.util.concurrent.TimeoutException

import akka.actor._
import akka.pattern.Patterns
import org.apache.commons.io.filefilter.TrueFileFilter
import org.apache.commons.io.{ FileUtils => IOFileUtils }
import org.ensime.config.EnsimeConfig
import org.ensime.protocol.{ FullTypeCheckCompleteEvent, AnalyzerReadyEvent, IndexerReadyEvent, ProtocolEvent }
import org.ensime.server.{ Project, Server }
import org.ensime.test.TestUtil
import org.ensime.util._
import org.scalatest.Assertions

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

  class AsyncMsgHelper(actorSystem: ActorSystem) {
    private case class AsyncSent(pe: ProtocolEvent)
    private case class AsyncRequest(pe: ProtocolEvent)

    private class AsyncMsgHelperActor extends Actor with ActorLogging {
      private var asyncMsgs = Map[ProtocolEvent, Int]()

      private var outstandingAsyncs = Vector[(ProtocolEvent, ActorRef)]()

      def processOutstandingRequests(): Unit = {
        outstandingAsyncs = outstandingAsyncs.filter {
          case (event, sender) =>
            asyncMsgs.get(event) match {
              case None =>
                true
              case Some(0) =>
                true
              case Some(n) =>
                asyncMsgs += (event -> (n - 1))
                sender ! None
                false
            }
        }
      }

      override def receive: Receive = {
        case AsyncRequest(req) =>
          outstandingAsyncs = outstandingAsyncs :+ (req, sender())
          processOutstandingRequests()
        case AsyncSent(event) =>
          val newCount = asyncMsgs.getOrElse(event, 0) + 1
          asyncMsgs = asyncMsgs + (event -> newCount)
          processOutstandingRequests()
      }
    }

    def asyncReceived(event: ProtocolEvent): Unit = {
      actor ! AsyncSent(event)
    }

    def expectAsync(dur: FiniteDuration, expected: ProtocolEvent) {

      val askRes = Patterns.ask(actor, AsyncRequest(expected), dur)
      try {
        Await.result(askRes, Duration.Inf)
      } catch {
        case t: TimeoutException =>
          fail("Timed out waiting for async msg: " + expected)
      }

    }

    private val actor = actorSystem.actorOf(Props(new AsyncMsgHelperActor()))
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
   * @param path The directory containing the test project (will not be modified)
   * @param f The test function to run
   */
  def withTestProject(path: String)(f: (EnsimeConfig, Project, AsyncMsgHelper) => Unit): Unit = {

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
      val project = server.project

      implicit val actorSystem = server.actorSystem

      val connInfo = project.rpcConnectionInfo()
      assert(connInfo.pid == None)
      assert(connInfo.serverName == "ENSIME-ReferenceServer")
      assert(connInfo.protocolVersion == "0.8.10")

      project.rpcNotifyClientReady()

      val asyncHelper = new AsyncMsgHelper(actorSystem)

      project.rpcSubscribeAsync(event => { asyncHelper.asyncReceived(event) })

      asyncHelper.expectAsync(60 seconds, AnalyzerReadyEvent) // compiler ready
      asyncHelper.expectAsync(60 seconds, FullTypeCheckCompleteEvent)
      asyncHelper.expectAsync(240 seconds, IndexerReadyEvent)

      f(config, server.project, asyncHelper)

      server.shutdown()
    }
  }
}

