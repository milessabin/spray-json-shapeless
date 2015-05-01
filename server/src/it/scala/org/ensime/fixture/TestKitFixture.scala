package org.ensime.fixture

import akka.actor.ActorSystem
import akka.testkit.{ ImplicitSender, TestKit }
import org.scalatest._

import scala.concurrent.duration._

/**
 * Normally a TestKit will reuse the same actor system for all tests
 * in a suite, but sometimes isolation of the system is needed on a
 * per-test basis, this fixture adds support for that.
 *
 * Instead of extending TestKit, use withActorSystem and import
 * the parameter for all implicits.
 *
 * Inspired by https://gist.github.com/derekwyatt/3138807
 */
trait TestKitFixture {
  require(
    !this.isInstanceOf[TestKit],
    "IsolatedActorSystems are incompatible with TestKit. Instead, 'import sys._'"
  )

  def withTestKit(actorSystem: ActorSystem)(testCode: TestKitFix => Any): Any
  def withTestKit(testCode: TestKitFix => Any): Any
}

class TestKitFix(actorSystem: ActorSystem) extends TestKit(actorSystem) with ImplicitSender
object TestKitFix {

  def apply(): TestKitFix = {
    new TestKitFix(ActorSystem("TestKitFix"))
  }

  def apply(actorSystem: ActorSystem) = {
    new TestKitFix(actorSystem)
  }
}

trait IsolatedTestKitFixture extends TestKitFixture {

  override def withTestKit(actorSystem: ActorSystem)(testCode: TestKitFix => Any): Any = {
    val sys = TestKitFix(actorSystem)
    testCode(sys)
  }

  override def withTestKit(testCode: TestKitFix => Any): Any = {
    val actorSystem = ActorSystem("IsolatedTestKitFixture")
    try {
      withTestKit(actorSystem)(testCode)
    } finally {
      actorSystem.shutdown()
      actorSystem.awaitTermination(10.seconds)
    }
  }
}

// this seems redundant, because it mimics "extends TestKit" behaviour,
// but it allows for easy swapping with the refreshing implementation
trait SharedTestKitFixture extends TestKitFixture with BeforeAndAfterAll {

  this: Suite =>

  private[fixture] var _testkit: TestKitFix = _
  private[fixture] var _actorSystem2: ActorSystem = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    _actorSystem2 = ActorSystem("SharedTestKitFixture")
    _testkit = TestKitFix(_actorSystem2)
  }

  override def afterAll(): Unit = {
    super.afterAll()
    _actorSystem2.shutdown()
    _actorSystem2.awaitTermination(10.seconds)
  }

  // TODO Need to check if we already have an actor system here.
  override def withTestKit(testCode: TestKitFix => Any): Any = testCode(_testkit)

}
