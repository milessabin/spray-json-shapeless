package org.ensime.fixture

import org.ensime.config._
import org.ensime.indexer.SearchService

trait SearchServiceFixture {
  def withSearchService(
    testCode: (EnsimeConfig, SearchService) => Any): Any

  def withSearchService(testCode: SearchService => Any): Any
}

trait IsolatedSearchServiceFixture extends SearchServiceFixture
    with IsolatedSourceResolverFixture {
  override def withSearchService(
    testCode: (EnsimeConfig, SearchService) => Any): Any = withSourceResolver { (config, resolver) =>
    testCode(config, new SearchService(config, resolver))
  }

  override def withSearchService(testCode: SearchService => Any): Any = withSourceResolver { (config, resolver) =>
    testCode(new SearchService(config, resolver))
  }
}

trait SharedSearchServiceFixture extends SearchServiceFixture
    with SharedSourceResolverFixture {
  private[fixture] var _search: SearchService = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    _search = new SearchService(_config, _resolver)
  }

  def withSearchService(
    testCode: (EnsimeConfig, SearchService) => Any): Unit = testCode(_config, _search)

  def withSearchService(testCode: SearchService => Any): Unit = testCode(_search)
}
