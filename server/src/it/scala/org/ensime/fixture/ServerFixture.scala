package org.ensime.fixture

import akka.actor._
import akka.pattern.{ AskTimeoutException, Patterns }
import org.ensime.config._
import org.ensime.core._
import org.ensime.server._

import scala.collection.immutable.ListMap
import scala.concurrent.Await
import scala.concurrent.duration._

object ServerFixture {
  private[fixture] def startup(config: EnsimeConfig)(implicit sys: ActorSystem): (Server, AsyncMsgHelper) = {

    val (server, readyFut) = Server.initialiseServer(config)

    val connInfo = server.project.connectionInfo()
    assert(connInfo.pid == None)
    assert(connInfo.implementation.name == "ENSIME")

    val asyncHelper = new AsyncMsgHelper(sys)

    server.project.subscribeAsync(event => { asyncHelper.asyncReceived(event) })

    asyncHelper.expectAsync(60 seconds, AnalyzerReadyEvent) // compiler ready
    asyncHelper.expectAsync(60 seconds, FullTypeCheckCompleteEvent)
    asyncHelper.expectAsync(240 seconds, IndexerReadyEvent)

    // this should be completed as all of the expected initialisation events have occurred.
    assert(readyFut.isCompleted)
    (server, asyncHelper)
  }
}

trait IsolatedServerFixture extends IsolatedEnsimeConfigFixture with IsolatedTestKitFixture {
  def withServer(testCode: (Server, AsyncMsgHelper) => Any)(implicit sys: ActorSystem): Any = {
    withTestKit { testkit =>
      withEnsimeConfig { config =>
        val (server, helper) = ServerFixture.startup(config)(sys)
        try testCode(server, helper)
        finally {
          server.shutdown()
        }
      }
    }
  }
}

trait SharedServerFixture extends SharedEnsimeConfigFixture
    with SharedTestKitFixture {

  this: SharedActorSystemFixture with SharedActorSystemFixture =>
  private var _server: Server = _
  private var _helper: AsyncMsgHelper = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    val (server, helper) = ServerFixture.startup(_config)(_actorSystem)
    _server = server
    _helper = helper
  }

  override def afterAll(): Unit = {
    _server.shutdown()
    super.afterAll()
  }

  def withServer[T](testCode: (Server, AsyncMsgHelper) => T): T =
    testCode(_server, _helper)
}

class AsyncMsgHelper(actorSystem: ActorSystem) {
  private case class AsyncSent(pe: EnsimeEvent)
  private case class AsyncRequest(pe: EnsimeEvent)
  private case class DumpState(pe: EnsimeEvent)

  private class AsyncMsgHelperActor extends Actor with ActorLogging {
    // ListMap instead of HashMap to avoid hashCode nonsense
    private var asyncMsgs = ListMap[EnsimeEvent, Int]().withDefaultValue(0)

    private var outstandingAsyncs = Vector[(EnsimeEvent, ActorRef)]()

    def processOutstandingRequests(): Unit = {
      outstandingAsyncs = outstandingAsyncs.filter {
        case (event, sender) =>
          asyncMsgs(event) match {
            case 0 => true
            case n =>
              val newCount = n - 1
              asyncMsgs += (event -> newCount)
              sender ! None
              newCount > 0
          }
      }
    }

    override def receive: Receive = {
      case AsyncRequest(req) =>
        log.info("Received request for " + req + " awaiting = " + asyncMsgs)
        outstandingAsyncs = outstandingAsyncs :+ (req, sender())
        processOutstandingRequests()
      case AsyncSent(event) =>
        log.info("Received event " + event)
        val newCount = asyncMsgs(event) + 1
        asyncMsgs = asyncMsgs + (event -> newCount)
        processOutstandingRequests()
      case DumpState(event) =>
        val msg = s"waiting for $event, unclaimed messages were: $outstandingAsyncs"
        log.warning(msg)
        println(msg) // should always be something we care about when this fails
    }
  }

  def asyncReceived(event: EnsimeEvent): Unit = {
    actor ! AsyncSent(event)
  }

  def expectAsync(dur: FiniteDuration, expected: EnsimeEvent): Unit = try {
    val askRes = Patterns.ask(actor, AsyncRequest(expected), dur)
    Await.result(askRes, Duration.Inf)
  } catch {
    case e: AskTimeoutException =>
      actor ! DumpState(expected)
      throw e
  }

  private val actor = actorSystem.actorOf(Props(new AsyncMsgHelperActor()))
}
