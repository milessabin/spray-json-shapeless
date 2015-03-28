package org.ensime.fixture

import org.scalatest._

import akka.actor.ActorSystem
import akka.testkit.ImplicitSender
import akka.testkit.TestKit

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

  def withTestKit(testCode: TestKitFix => Any): Any
}

class TestKitFix extends TestKit(ActorSystem()) with ImplicitSender

// this seems redundant, because it mimics "extends TestKit" behaviour,
// but it allows for easy swapping with the refreshing implementation
trait SharedTestKitFixture extends TestKitFixture with BeforeAndAfterAll {
  this: Suite =>

  private[fixture] var _testkit: TestKitFix = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    _testkit = new TestKitFix
  }

  override def afterAll(): Unit = {
    super.afterAll()
    _testkit.system.shutdown()
  }

  override def withTestKit(testCode: TestKitFix => Any): Any = testCode(_testkit)

}
