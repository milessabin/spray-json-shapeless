package org.ensime.fixture

import akka.actor.ActorSystem
import org.ensime.config._
import org.ensime.indexer.SearchService
import scala.concurrent.duration._

trait SearchServiceFixture {
  def withSearchService(
    testCode: (ActorSystem, EnsimeConfig, SearchService) => Any
  ): Any

  def withSearchService(testCode: SearchService => Any): Any
}

trait IsolatedSearchServiceFixture extends SearchServiceFixture
    with IsolatedSourceResolverFixture {
  override def withSearchService(
    testCode: (ActorSystem, EnsimeConfig, SearchService) => Any
  ): Any = withSourceResolver { (config, resolver) =>
    val actorSystem = ActorSystem("IsolatedSearchServiceFixture")
    val searchService = new SearchService(config, resolver, actorSystem)
    try {

      testCode(actorSystem, config, searchService)
    } finally {
      searchService.shutdown()
      actorSystem.shutdown()
      actorSystem.awaitTermination(10.seconds)
    }
  }

  override def withSearchService(testCode: SearchService => Any): Any = withSourceResolver { (config, resolver) =>
    val actorSystem = ActorSystem("IsolatedSearchServiceFixture")
    val searchService = new SearchService(config, resolver, actorSystem)
    try {
      testCode(searchService)
    } finally {
      searchService.shutdown()
      actorSystem.shutdown()
      actorSystem.awaitTermination(10.seconds)
    }
  }
}

trait SharedSearchServiceFixture extends SearchServiceFixture
    with SharedSourceResolverFixture {
  private[fixture] var _actorSystem: ActorSystem = _
  private[fixture] var _search: SearchService = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    _actorSystem = ActorSystem("SharedSearchServiceFixture")
    _search = new SearchService(_config, _resolver, _actorSystem)
  }

  override def afterAll(): Unit = {
    _search.shutdown()
    _actorSystem.shutdown()
  }

  override def withSearchService(
    testCode: (ActorSystem, EnsimeConfig, SearchService) => Any
  ): Unit = testCode(_actorSystem, _config, _search)

  override def withSearchService(testCode: SearchService => Any): Unit = testCode(_search)
}
