package org.ensime.fixture

import org.ensime.config._
import org.ensime.indexer.SourceResolver

trait SourceResolverFixture {
  def withSourceResolver(testCode: SourceResolver => Any): Any
  def withSourceResolver(testCode: (EnsimeConfig, SourceResolver) => Any): Any
}

trait IsolatedSourceResolverFixture extends SourceResolverFixture
    with IsolatedEnsimeConfigFixture {
  override def withSourceResolver(testCode: SourceResolver => Any): Any = withEnsimeConfig { config =>
    testCode(new SourceResolver(config))
  }
  override def withSourceResolver(testCode: (EnsimeConfig, SourceResolver) => Any): Any = withEnsimeConfig { config =>
    testCode(config, new SourceResolver(config))
  }
}

trait SharedSourceResolverFixture extends SourceResolverFixture
    with SharedEnsimeConfigFixture {
  private[fixture] var _resolver: SourceResolver = _
  override def beforeAll(): Unit = {
    super.beforeAll()
    _resolver = new SourceResolver(_config)
  }

  override def withSourceResolver(testCode: SourceResolver => Any): Any = testCode(_resolver)
  override def withSourceResolver(testCode: (EnsimeConfig, SourceResolver) => Any): Any = {
    testCode(_config, _resolver)
  }

}
