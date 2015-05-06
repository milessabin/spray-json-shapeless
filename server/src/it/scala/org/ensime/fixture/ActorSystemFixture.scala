package org.ensime.fixture

import akka.actor.ActorSystem
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
trait ActorSystemFixture {
  def withActorSystem(testCode: ActorSystem => Any): Any
}

trait IsolatedActorSystemFixture extends ActorSystemFixture {

  override def withActorSystem(testCode: ActorSystem => Any): Any = {
    val actorSystem = ActorSystem("IsolatedActorSystemFixture")
    try {
      testCode(actorSystem)
    } finally {
      actorSystem.shutdown()
      actorSystem.awaitTermination(10.seconds)
    }
  }
}

// this seems redundant, because it mimics "extends TestKit" behaviour,
// but it allows for easy swapping with the refreshing implementation
trait SharedActorSystemFixture extends ActorSystemFixture with BeforeAndAfterAll {

  this: Suite =>

  private[fixture] var _actorSystem: ActorSystem = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    _actorSystem = ActorSystem("SharedActorSystemFixture")
  }

  override def afterAll(): Unit = {
    super.afterAll()
    _actorSystem.shutdown()
    _actorSystem.awaitTermination(10.seconds)
  }

  override def withActorSystem(testCode: ActorSystem => Any): Any = testCode(_actorSystem)
}
