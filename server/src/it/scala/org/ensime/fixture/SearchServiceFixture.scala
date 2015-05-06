package org.ensime.fixture

import akka.actor.ActorSystem
import org.ensime.config._
import org.ensime.indexer.{ EnsimeVFS, SearchService }
import scala.concurrent.duration._

trait IsolatedSearchServiceFixture extends IsolatedSourceResolverFixture {

  def withSearchService(testCode: (EnsimeConfig, SearchService) => Any)(implicit actorSystem: ActorSystem): Any = withSourceResolver { (config, resolver) =>
    val ensimeVFS = EnsimeVFS()
    val searchService = new SearchService(config, resolver, actorSystem, ensimeVFS)
    try {

      testCode(config, searchService)
    } finally {
      searchService.shutdown()
      ensimeVFS.close()
      actorSystem.shutdown()
      actorSystem.awaitTermination(10.seconds)
    }
  }
}

trait SharedSearchServiceFixture extends SharedEnsimeVFSFixture
    with SharedSourceResolverFixture {

  this: SharedActorSystemFixture =>
  private[fixture] var _search: SearchService = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    _search = new SearchService(_config, _resolver, _actorSystem, _vfs)
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
