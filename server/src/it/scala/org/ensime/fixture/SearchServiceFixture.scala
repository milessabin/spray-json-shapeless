package org.ensime.fixture

import akka.actor.ActorSystem
import org.ensime.api._
import org.ensime.indexer.{ EnsimeVFS, SearchService }
import scala.concurrent.duration._

trait IsolatedSearchServiceFixture extends IsolatedSourceResolverFixture {

  def withSearchService(testCode: (EnsimeConfig, SearchService) => Any)(implicit actorSystem: ActorSystem, vfs: EnsimeVFS): Any = withSourceResolver { (config, resolver) =>
    val searchService = new SearchService(config, resolver)
    try {
      testCode(config, searchService)
    } finally {
      searchService.shutdown()
      actorSystem.shutdown()
      actorSystem.awaitTermination(10.seconds)
    }
  }
}

trait SharedSearchServiceFixture
    extends SharedEnsimeVFSFixture
    with SharedSourceResolverFixture {
  this: SharedTestKitFixture =>

  private[fixture] var _search: SearchService = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    implicit val system = _testkit.system
    _search = new SearchService(_config, _resolver)
  }

  override def afterAll(): Unit = {
    _search.shutdown()
    super.afterAll()
  }

  def withSearchService(
    testCode: (EnsimeConfig, SearchService) => Any
  ): Unit = testCode(_config, _search)

  def withSearchService(testCode: SearchService => Any): Unit = testCode(_search)
}
