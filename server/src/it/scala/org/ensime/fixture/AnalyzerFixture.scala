package org.ensime.fixture

import akka.actor._
import akka.testkit._
import org.ensime.api._
import org.ensime.core._
import org.ensime.indexer.{ EnsimeVFS, SearchService }
import org.scalatest._

trait AnalyzerFixture {
  def withAnalyzer(testCode: (EnsimeConfig, TestActorRef[Analyzer]) => Any): Any
}

object AnalyzerFixture {
  private[fixture] def create(config: EnsimeConfig, search: SearchService)(implicit system: ActorSystem, vfs: EnsimeVFS): TestActorRef[Analyzer] = {
    val indexer = TestProbe()
    val projectActor = TestProbe()
    TestActorRef(Props(
      new Analyzer(projectActor.ref, indexer.ref, search, config)
    ))
  }
}

trait IsolatedAnalyzerFixture
    extends AnalyzerFixture
    with IsolatedEnsimeVFSFixture
    with IsolatedActorSystemFixture
    with IsolatedSearchServiceFixture
    with IsolatedTestKitFixture {

  override def withAnalyzer(testCode: (EnsimeConfig, TestActorRef[Analyzer]) => Any): Any = {
    withVFS { implicit vfs =>
      withActorSystem { implicit actorSystem =>
        withTestKit { testkit =>
          withSearchService { (config, search) =>
            testCode(config, AnalyzerFixture.create(config, search))
          }
        }
      }
    }
  }

}

trait SharedAnalyzerFixture
    extends AnalyzerFixture
    with SharedActorSystemFixture
    with SharedTestKitFixture
    with SharedSearchServiceFixture
    with BeforeAndAfterAll {

  private[fixture] var analyzer: TestActorRef[Analyzer] = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    analyzer = AnalyzerFixture.create(_config, _search)
  }

  override def withAnalyzer(testCode: (EnsimeConfig, TestActorRef[Analyzer]) => Any): Any = testCode(_config, analyzer)
}
