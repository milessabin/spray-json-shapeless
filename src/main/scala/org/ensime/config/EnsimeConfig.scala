package org.ensime.config

import akka.event.slf4j.SLF4JLogging
import java.io.File
import org.ensime.server.protocol.swank.sexp._
import org.ensime.server.protocol.swank.sexp.formats._
import org.ensime.util._
import pimpathon.file._
import scalariform.formatter.preferences.FormattingPreferences
import RichFile._
import FileUtils.jdkDir

case class EnsimeConfig(
    `root-dir`: File,
    `cache-dir`: File,
    name: String,
    `scala-version`: String,
    `compiler-args`: List[String],
    `reference-source-roots`: List[File],
    subprojects: List[EnsimeModule],
    `formatting-prefs`: FormattingPreferences,
    `source-mode`: Boolean,
    `debug-args`: List[String]) extends SLF4JLogging {
  (`root-dir` :: `cache-dir` :: referenceSourceJars).foreach { f =>
    require(f.exists, "" + f + " is required but does not exist")
  }

  // convertors between the "legacy" names and the preferred ones
  // (with fallback defaults)
  def root = `root-dir`
  def cacheDir = `cache-dir`
  def scalaVersion = `scala-version`
  def compilerArgs = `compiler-args`
  val modules = subprojects.map { module => (module.name, module) }.toMap
  def referenceSourceJars = `reference-source-roots`
  val formattingPrefs = `formatting-prefs`
  val sourceMode = `source-mode`
  def debugVMArgs = `debug-args`

  private[config] def validated(): EnsimeConfig = {
    copy(subprojects = subprojects.map(_.validated()))
  }

  def sourceFiles: Set[File] = for {
    module: EnsimeModule <- modules.values.toSet
    root <- module.sourceRoots
    file <- root.tree
    if file.isFile & file.getName.endsWith(".scala")
  } yield file

  def runtimeClasspath: Set[File] =
    compileClasspath ++ modules.values.flatMap(_.debugJars) ++ targetClasspath

  def compileClasspath: Set[File] = modules.values.toSet.flatMap {
    m: EnsimeModule => m.compileJars ++ m.testJars
  } ++ (if (sourceMode) List.empty else targetClasspath)

  def targetClasspath: Set[File] = modules.values.toSet.flatMap {
    m: EnsimeModule => m.targetDirs ++ m.testTargetDirs
  }

  val javaLib = jdkDir / "jre/lib/rt.jar"
  require(javaLib.exists, "The JRE does not exist at the expected location " + javaLib)

  def allJars: Set[File] = {
    modules.values.flatMap { m =>
      m.compileJars ::: m.testJars
    }.toSet
  } + javaLib
}

case class EnsimeModule(
    name: String,
    target: Option[File],
    targets: List[File],
    `test-target`: Option[File],
    `test-targets`: List[File],
    `depends-on-modules`: List[String],
    `compile-deps`: List[File],
    `runtime-deps`: List[File],
    `test-deps`: List[File],
    `source-roots`: List[File],
    `reference-source-roots`: List[File]) extends SLF4JLogging {
  // only check the files, not the directories, see below
  (`compile-deps` ::: `runtime-deps` :::
    `test-deps` ::: `reference-source-roots`).foreach { f =>
      require(f.exists, "" + f + " is required but does not exist")
    }
  // the preferred accessors
  val targetDirs = targets ++ target.toIterable
  val testTargetDirs = `test-targets` ++ `test-target`.toIterable
  def dependsOnNames = `depends-on-modules`
  def compileJars = `compile-deps`
  def debugJars = `runtime-deps` // yuck
  def testJars = `test-deps`
  def sourceRoots = `source-roots`
  def referenceSourcesJars = `reference-source-roots`

  def dependencies(implicit config: EnsimeConfig): List[EnsimeModule] =
    dependsOnNames.map(config.modules)

  /*
   We use the canonical form of files/directories to keep OS X happy
   when loading S-Expressions. But the canon may fail to resolve if
   the file/directory does not exist, so we force create all required
   directories and then re-canon them, which is - admittedly - a weird
   side-effect.
   */
  private[config] def validated(): EnsimeModule = {
    (targetDirs ++ testTargetDirs ++ sourceRoots).foreach { dir =>
      if (!dir.exists()) {
        log.warn("" + dir + " does not exist, creating")
        dir.mkdirs()
      }
    }
    copy(
      target = None,
      targets = targetDirs.map(_.canon),
      `test-target` = None,
      `test-targets` = testTargetDirs.map(_.canon),
      `source-roots` = sourceRoots.map(_.canon)
    )
  }
}

object EnsimeConfig {

  // TODO This protocol code should move into server.protocol
  // we customise how some basic object types are handled
  object Protocol extends DefaultSexpProtocol
    with OptionAltFormat
    with CanonFileFormat
    with ScalariformFormat
  import Protocol._

  private implicit val moduleFormat = SexpFormat[EnsimeModule]
  private implicit val configFormat = SexpFormat[EnsimeConfig]

  def parse(config: String): EnsimeConfig = {
    val raw = config.parseSexp.convertTo[EnsimeConfig]
    raw.validated()
  }
}
