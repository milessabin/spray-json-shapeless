package org.ensime.fixture

import java.io.File

import org.apache.commons.io.FileUtils.copyDirectory
import org.ensime.api._
import org.ensime.config._
import org.scalatest._
import pimpathon.any._
import pimpathon.file._

/**
 * Provides a fixture for tests to have access to a cloned project,
 * based on an example project that will be untouched.
 */
trait EnsimeConfigFixture {
  /** The definition of the original project to clone for testing. */
  def original: EnsimeConfig

  def withEnsimeConfig(testCode: EnsimeConfig => Any): Any

  // convenience method
  def scalaMain(implicit config: EnsimeConfig): File =
    config.subprojects.head.sourceRoots.filter { dir =>
      val sep = File.separator
      dir.getPath.endsWith(s"${sep}main${sep}scala")
    }.head
}

object EnsimeConfigFixture {

  lazy val dotEnsime = file("../.ensime")
  require(
    dotEnsime.exists,
    "the .ensime file must exist to run the integration tests." +
      "Type 'sbt gen-ensime' to create it"
  )

  lazy val EnsimeTestProject = EnsimeConfigProtocol.parse(dotEnsime.readLines().mkString)

  // not completely empty, has a reference to the scala-library jar
  lazy val EmptyTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testingEmpty"),
    javaLibs = Nil
  )
  lazy val SimpleTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testingSimple")
  )
  lazy val DebugTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testingDebug")
  )
  lazy val DocsTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testingDocs")
  )

  // generates an empty single module project in a temporary directory
  // and returns the config, containing many of the same settings
  // as the ensime-server project itself (source/dependency jars),
  // with options to copy ENSIME's own sources/classes into the structure.
  def cloneForTesting(
    source: EnsimeConfig,
    target: File
  ): EnsimeConfig = {

    def rename(from: File): File = {
      val toPath = from.getAbsolutePath.replace(
        source.root.getAbsolutePath,
        target.getAbsolutePath
      )
      require(toPath != from.getAbsolutePath, s"${source.root.getAbsolutePath} ${target.getAbsolutePath} in ${from.getAbsolutePath}")
      file(toPath)
    }

    def renameAndCopy(from: File): File = rename(from) withSideEffect {
      to => copyDirectory(from, to)
    }

    // I tried using shapeless everywhere here, but it OOMd the compiler :-(

    def cloneModule(m: EnsimeModule): EnsimeModule = m.copy(
      target = m.target.map(renameAndCopy),
      targets = m.targets.map(renameAndCopy),
      testTarget = m.testTarget.map(renameAndCopy),
      testTargets = m.testTargets.map(renameAndCopy),
      sourceRoots = m.sourceRoots.map(renameAndCopy)
    )

    val cacheDir = rename(source.cacheDir)
    cacheDir.mkdirs()
    val config = EnsimeConfigProtocol.validated(source.copy(
      rootDir = rename(source.rootDir),
      cacheDir = cacheDir,
      subprojects = source.subprojects.map(cloneModule)
    ))

    // HACK: we must force unix line endings on sources or the tests
    // (which have fixed points within the file) will fail on Windows
    config.sourceFiles.foreach { file =>
      file.writeLines(file.readLines(), append = false)
    }

    config
  }
}

/**
 * Provides the basic building blocks to build custom fixtures around
 * a project that is cloned for every test in a suite.
 *
 * Implementations tend to run very slowly, so consider using
 * `SharedConfigFixture` if possible, or reducing your configuration
 * parameters to the bare minimal (e.g. remove JRE and dependencies to
 * index if not needed).
 */
trait IsolatedEnsimeConfigFixture extends Suite
    with EnsimeConfigFixture {
  //    with ParallelTestExecution {
  // careful: ParallelTestExecution is causing weird failures:
  //   https://github.com/sbt/sbt/issues/1890
  //
  // also, Jenkins doesn't like it:
  //   https://github.com/scoverage/sbt-scoverage/issues/97
  import EnsimeConfigFixture._

  override def withEnsimeConfig(testCode: EnsimeConfig => Any): Any = withTempDirectory(
    ".project", s"ensime-it-${getClass.getSimpleName}-"
  ) {
      dir => testCode(cloneForTesting(original, dir))
    }
}

/**
 * Provides the basic building blocks to build custom fixtures around
 * a project that is cloned once for the test suite.
 */
trait SharedEnsimeConfigFixture extends Suite
    with EnsimeConfigFixture with BeforeAndAfterAll {
  import EnsimeConfigFixture._

  private val tmpDir = tempDir(".project", s"ensime-it-${getClass.getSimpleName}-")

  private[fixture] var _config: EnsimeConfig = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    _config = cloneForTesting(original, tmpDir)
  }

  override def afterAll(): Unit = {
    super.afterAll()
    tmpDir.deleteRecursively()
  }

  override def withEnsimeConfig(testCode: EnsimeConfig => Any): Any = testCode(_config)

}
