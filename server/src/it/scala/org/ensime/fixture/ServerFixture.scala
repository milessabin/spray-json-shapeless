package org.ensime.fixture

import akka.actor._
import akka.pattern.Patterns
import org.ensime.config._
import org.ensime.core._
import org.ensime.server._

import scala.concurrent.Await
import scala.concurrent.duration._

trait ServerFixture {
  def withServer(testCode: (Server, AsyncMsgHelper) => Any): Any
}

object ServerFixture {
  private[fixture] def startup(config: EnsimeConfig)(implicit sys: ActorSystem): (Server, AsyncMsgHelper) = {
    val server = Server.initialiseServer(config)

    val connInfo = server.project.rpcConnectionInfo()
    assert(connInfo.pid == None)
    assert(connInfo.implementation.name == "ENSIME")

    server.project.rpcNotifyClientReady()

    val asyncHelper = new AsyncMsgHelper(sys)

    server.project.rpcSubscribeAsync(event => { asyncHelper.asyncReceived(event) })

    asyncHelper.expectAsync(60 seconds, AnalyzerReadyEvent) // compiler ready
    asyncHelper.expectAsync(60 seconds, FullTypeCheckCompleteEvent)
    asyncHelper.expectAsync(240 seconds, IndexerReadyEvent)

    (server, asyncHelper)
  }
}

trait IsolatedServerFixture extends ServerFixture
    with IsolatedEnsimeConfigFixture with IsolatedTestKitFixture {
  override def withServer(testCode: (Server, AsyncMsgHelper) => Any): Any = withTestKit { testkit =>
    withEnsimeConfig { config =>
      val (server, helper) = ServerFixture.startup(config)(testkit.system)
      try testCode(server, helper)
      finally {
        server.shutdown()
      }
    }
  }
}

trait SharedServerFixture extends ServerFixture
    with SharedEnsimeConfigFixture with SharedTestKitFixture {

  private var _server: Server = _
  private var _helper: AsyncMsgHelper = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    val (server, helper) = ServerFixture.startup(_config)(_testkit.system)
    _server = server
    _helper = helper
  }

  override def afterAll(): Unit = {
    super.afterAll()
    _server.shutdown()
  }

  override def withServer(testCode: (Server, AsyncMsgHelper) => Any): Any =
    testCode(_server, _helper)
}

class AsyncMsgHelper(actorSystem: ActorSystem) {
  private case class AsyncSent(pe: EnsimeEvent)
  private case class AsyncRequest(pe: EnsimeEvent)

  private class AsyncMsgHelperActor extends Actor with ActorLogging {
    private var asyncMsgs = Map[EnsimeEvent, Int]() withDefaultValue (0)

    private var outstandingAsyncs = Vector[(EnsimeEvent, ActorRef)]()

    def processOutstandingRequests(): Unit = {
      outstandingAsyncs = outstandingAsyncs.filter {
        case (event, sender) =>
          asyncMsgs(event) match {
            case 0 => true
            case n =>
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
        log.debug("received handled message: " + event)
        val newCount = asyncMsgs(event) + 1
        asyncMsgs = asyncMsgs + (event -> newCount)
        processOutstandingRequests()
    }
  }

  def asyncReceived(event: EnsimeEvent): Unit = {
    actor ! AsyncSent(event)
  }

  def expectAsync(dur: FiniteDuration, expected: EnsimeEvent): Unit = {
    val askRes = Patterns.ask(actor, AsyncRequest(expected), dur)
    Await.result(askRes, Duration.Inf)
  }

  private val actor = actorSystem.actorOf(Props(new AsyncMsgHelperActor()))
}
