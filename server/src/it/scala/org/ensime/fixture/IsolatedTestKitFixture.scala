package org.ensime.fixture

trait IsolatedTestKitFixture extends TestKitFixture {
  override def withTestKit(testCode: TestKitFix => Any): Any = {
    val sys = new TestKitFix
    try {
      testCode(sys)
    } finally {
      sys.system.shutdown()
    }
  }
}