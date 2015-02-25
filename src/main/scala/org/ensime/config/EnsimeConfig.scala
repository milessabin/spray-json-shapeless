package org.ensime.config

import akka.event.slf4j.SLF4JLogging
import java.io.File
import org.ensime.sexp._
import org.ensime.sexp.formats._
import org.ensime.util._
import pimpathon.file._
import scalariform.formatter.preferences.FormattingPreferences
import RichFile._
import FileUtils.jdkDir

case class EnsimeConfig(
    rootDir: File,
    cacheDir: File,
    name: String,
    scalaVersion: String,
    compilerArgs: List[String],
    referenceSourceRoots: List[File],
    subprojects: List[EnsimeModule],
    formattingPrefs: FormattingPreferences,
    sourceMode: Boolean,
    debugArgs: List[String]) extends SLF4JLogging {
  (rootDir :: cacheDir :: referenceSourceRoots).foreach { f =>
    require(f.exists, "" + f + " is required but does not exist")
  }

  /*
   Proposed alternatives to the legacy wire format field names:
   */
  def root = rootDir
  def debugVMArgs = debugArgs
  def referenceSourceJars = referenceSourceRoots

  // some marshalling libs (e.g. spray-json) might not like extra vals
  val modules = subprojects.map { module => (module.name, module) }.toMap
  val javaLib = jdkDir / "jre/lib/rt.jar"
  require(javaLib.exists, "The JRE does not exist at the expected location " + javaLib)

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
    compileClasspath ++ modules.values.flatMap(_.runtimeDeps) ++ targetClasspath

  def compileClasspath: Set[File] = modules.values.toSet.flatMap {
    m: EnsimeModule => m.compileDeps ++ m.testDeps
  } ++ (if (sourceMode) List.empty else targetClasspath)

  def targetClasspath: Set[File] = modules.values.toSet.flatMap {
    m: EnsimeModule => m.targetDirs ++ m.testTargetDirs
  }

  def allJars: Set[File] = {
    modules.values.flatMap { m =>
      m.compileDeps ::: m.testDeps
    }.toSet
  } + javaLib
}

case class EnsimeModule(
    name: String,
    target: Option[File],
    targets: List[File],
    testTarget: Option[File],
    testTargets: List[File],
    dependsOnModules: List[String],
    compileDeps: List[File],
    runtimeDeps: List[File],
    testDeps: List[File],
    sourceRoots: List[File],
    docJars: List[File],
    referenceSourceRoots: List[File]) extends SLF4JLogging {
  // only check the files, not the directories, see below
  (compileDeps ::: runtimeDeps :::
    testDeps ::: referenceSourceRoots).foreach { f =>
      require(f.exists, "" + f + " is required but does not exist")
    }

  /*
   Proposed alternatives to the legacy wire format field names:
   */
  def compileJars = compileDeps
  def testJars = testDeps
  def referenceSourceJars = referenceSourceRoots

  // prefer these to the raw target(s)
  val targetDirs = targets ++ target.toIterable
  val testTargetDirs = testTargets ++ testTarget.toIterable

  def dependencies(implicit config: EnsimeConfig): List[EnsimeModule] =
    dependsOnModules.map(config.modules)

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
      testTarget = None,
      testTargets = testTargetDirs.map(_.canon),
      sourceRoots = sourceRoots.map(_.canon)
    )
  }
}

object EnsimeConfig {
  object Protocol extends DefaultSexpProtocol
    with OptionAltFormat
    with CanonFileFormat
    with ScalariformFormat
    with CamelCaseToDashes
  import Protocol._

  private implicit val moduleFormat = SexpFormat[EnsimeModule]
  private implicit val configFormat = SexpFormat[EnsimeConfig]

  def parse(config: String): EnsimeConfig = {
    val raw = config.parseSexp.convertTo[EnsimeConfig]
    raw.validated()
  }
}
