package org.ensime.fixture

import org.ensime.api._
import org.ensime.indexer.{ EnsimeVFS, SourceResolver }

trait SourceResolverFixture {
  def withSourceResolver(testCode: SourceResolver => Any): Any
  def withSourceResolver(testCode: (EnsimeConfig, SourceResolver) => Any): Any
}

trait IsolatedSourceResolverFixture extends SourceResolverFixture
    with IsolatedEnsimeConfigFixture {
  override def withSourceResolver(testCode: SourceResolver => Any): Any = withEnsimeConfig { config =>
    implicit val vfs = EnsimeVFS()
    try {
      testCode(new SourceResolver(config))
    } finally {
      vfs.close()
    }
  }
  override def withSourceResolver(testCode: (EnsimeConfig, SourceResolver) => Any): Any = withEnsimeConfig { config =>
    implicit val vfs = EnsimeVFS()
    try {
      testCode(config, new SourceResolver(config))
    } finally {
      vfs.close()
    }
  }
}

trait SharedSourceResolverFixture extends SourceResolverFixture
    with SharedEnsimeConfigFixture {
  this: SharedEnsimeVFSFixture =>

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
