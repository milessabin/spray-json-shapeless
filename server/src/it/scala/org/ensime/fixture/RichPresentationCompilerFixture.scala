package org.ensime.fixture

import java.io.File

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import org.ensime.config._
import org.ensime.core._
import org.ensime.indexer.SearchService
import org.slf4j.LoggerFactory

import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.StoreReporter

trait RichPresentationCompilerFixture {
  def withRichPresentationCompiler(
    testCode: (TestKitFix, EnsimeConfig, RichPresentationCompiler) => Any): Any
}

object RichPresentationCompilerFixture {
  private[fixture] def create(
    config: EnsimeConfig,
    search: SearchService)(implicit system: ActorSystem): RichPresentationCompiler = {
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
    with IsolatedTestKitFixture
    with IsolatedSearchServiceFixture {

  override def withRichPresentationCompiler(
    testCode: (TestKitFix, EnsimeConfig, RichPresentationCompiler) => Any): Any = withSearchService { (config, search) =>
    withTestKit { testkit =>
      import org.ensime.fixture.RichPresentationCompilerFixture._
      testCode(testkit, config, create(config, search)(testkit.system))
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
    pc = create(_config, _search)(_testkit.system)
  }

  override def withRichPresentationCompiler(
    testCode: (TestKitFix, EnsimeConfig, RichPresentationCompiler) => Any): Any = testCode(_testkit, _config, pc)
}
