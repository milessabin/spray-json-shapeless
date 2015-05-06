package org.ensime.fixture

import akka.actor.ActorSystem
import akka.testkit.{ ImplicitSender, TestKit }
import org.scalatest._

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

  def withTestKit(testCode: TestKitFix => Any)(implicit actorSystem: ActorSystem): Any
}

class TestKitFix(actorSystem: ActorSystem) extends TestKit(actorSystem) with ImplicitSender
object TestKitFix {

  def apply(actorSystem: ActorSystem) = {
    new TestKitFix(actorSystem)
  }
}

trait IsolatedTestKitFixture extends TestKitFixture {

  override def withTestKit(testCode: TestKitFix => Any)(implicit actorSystem: ActorSystem): Any = {
    testCode(TestKitFix(actorSystem))
  }
}

// this seems redundant, because it mimics "extends TestKit" behaviour,
// but it allows for easy swapping with the refreshing implementation
trait SharedTestKitFixture extends TestKitFixture with BeforeAndAfterAll {

  this: Suite with SharedActorSystemFixture =>

  private[fixture] var _testkit: TestKitFix = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    _testkit = TestKitFix(_actorSystem)
  }

  override def afterAll(): Unit = {
    super.afterAll()
  }

  def withTestKit(testCode: TestKitFix => Any)(implicit actorSystem: ActorSystem): Any = {
    testCode(_testkit)
  }
}
