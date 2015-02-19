package org.ensime.fixture

import akka.actor._
import org.ensime.indexer.SearchService
import org.scalatest._
import akka.testkit._

import org.ensime.config._
import org.ensime.core._

trait AnalyzerFixture {
  def withAnalyzer(testCode: (EnsimeConfig, TestActorRef[Analyzer]) => Any): Any
}

object AnalyzerFixture {
  private[fixture] def create(config: EnsimeConfig, search: SearchService)(implicit system: ActorSystem): TestActorRef[Analyzer] = {
    val indexer = TestProbe()
    val projectActor = TestProbe()
    TestActorRef(Props(
      new Analyzer(projectActor.ref, indexer.ref, search, config)
    ))
  }
}

trait IsolatedAnalyzerFixture
    extends AnalyzerFixture
    with IsolatedSearchServiceFixture
    with IsolatedTestKitFixture {
  import AnalyzerFixture._

  override def withAnalyzer(testCode: (EnsimeConfig, TestActorRef[Analyzer]) => Any): Any = withSearchService { (config, search) =>
    withTestKit { testkit =>
      testCode(config, AnalyzerFixture.create(config, search)(testkit.system))
    }
  }

}

trait SharedAnalyzerFixture
    extends AnalyzerFixture
    with SharedSearchServiceFixture
    with SharedTestKitFixture
    with BeforeAndAfterAll {
  private[fixture] var analyzer: TestActorRef[Analyzer] = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    analyzer = AnalyzerFixture.create(_config, _search)(_testkit.system)
  }

  override def withAnalyzer(testCode: (EnsimeConfig, TestActorRef[Analyzer]) => Any): Any = testCode(_config, analyzer)

}
