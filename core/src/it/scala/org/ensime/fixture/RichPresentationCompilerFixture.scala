package org.ensime.fixture

import java.io.File

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import org.ensime.api._
import org.ensime.core._
import org.ensime.indexer._
import org.slf4j.LoggerFactory

import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.StoreReporter

trait RichPresentationCompilerFixture {
  def withRichPresentationCompiler(
    testCode: (TestKitFix, EnsimeConfig, RichPresentationCompiler) => Any
  ): Any
}

object RichPresentationCompilerFixture {
  private[fixture] def create(
    config: EnsimeConfig,
    search: SearchService
  )(
    implicit
    system: ActorSystem,
    vfs: EnsimeVFS
  ): RichPresentationCompiler = {
    val scalaLib = config.allJars.find(_.getName.contains("scala-library")).get

    val presCompLog = LoggerFactory.getLogger(classOf[Global])
    val settings = new Settings(presCompLog.error)
    settings.YpresentationDebug.value = presCompLog.isTraceEnabled
    settings.YpresentationVerbose.value = presCompLog.isDebugEnabled
    settings.verbose.value = presCompLog.isDebugEnabled
    //settings.usejavacp.value = true
    settings.bootclasspath.append(scalaLib.getAbsolutePath)
    settings.classpath.value = config.compileClasspath.mkString(File.pathSeparator)

    val reporter = new StoreReporter()
    val indexer = TestProbe()
    val parent = TestProbe()

    new RichPresentationCompiler(
      config, settings, reporter, parent.ref, indexer.ref, search
    )
  }
}

trait IsolatedRichPresentationCompilerFixture
    extends RichPresentationCompilerFixture
    with IsolatedEnsimeVFSFixture
    with IsolatedTestKitFixture
    with IsolatedSearchServiceFixture {

  override def withRichPresentationCompiler(
    testCode: (TestKitFix, EnsimeConfig, RichPresentationCompiler) => Any
  ): Any = {
    withVFS { implicit vfs =>
      withTestKit { testkit =>
        import testkit._
        withSearchService { (config, search) =>
          import org.ensime.fixture.RichPresentationCompilerFixture._
          val pc = create(config, search)
          try {
            testCode(testkit, config, pc)
          } finally {
            pc.askShutdown()
          }
        }
      }
    }
  }

}

trait SharedRichPresentationCompilerFixture
    extends RichPresentationCompilerFixture
    with SharedTestKitFixture
    with SharedSearchServiceFixture {

  private[fixture] var pc: RichPresentationCompiler = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    import org.ensime.fixture.RichPresentationCompilerFixture._
    implicit val system = _testkit.system
    pc = create(_config, _search)
  }

  override def withRichPresentationCompiler(
    testCode: (TestKitFix, EnsimeConfig, RichPresentationCompiler) => Any
  ): Any = testCode(_testkit, _config, pc)
}
