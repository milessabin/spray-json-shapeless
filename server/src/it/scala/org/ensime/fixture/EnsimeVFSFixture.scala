package org.ensime.fixture

import org.ensime.indexer.EnsimeVFS
import org.scalatest.{ BeforeAndAfterAll, Suite }

trait EnsimeVFSFixture {

  def withVFS[T](testCode: EnsimeVFS => T): T
}

trait IsolatedEnsimeVFSFixture extends Suite with EnsimeVFSFixture {

  override def withVFS[T](testCode: EnsimeVFS => T): T = {
    val vfs = EnsimeVFS()
    try {
      testCode(vfs)
    } finally {
      vfs.close()
    }
  }
}

/**
 * Provides the basic building blocks to build custom fixtures around
 * a project that is cloned once for the test suite.
 */
trait SharedEnsimeVFSFixture extends Suite
    with EnsimeVFSFixture with BeforeAndAfterAll {
  private[fixture] implicit var _vfs: EnsimeVFS = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    _vfs = EnsimeVFS()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    _vfs.close()
  }

  override def withVFS[T](testCode: EnsimeVFS => T): T = {
    testCode(_vfs)
  }
}
