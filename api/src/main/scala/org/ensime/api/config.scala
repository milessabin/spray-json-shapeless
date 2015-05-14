package org.ensime.api

import java.io.File
import scalariform.formatter.preferences.FormattingPreferences

import pimpathon.file._

case class EnsimeConfig(
    rootDir: File,
    cacheDir: File,
    javaHome: File,
    name: String,
    scalaVersion: String,
    compilerArgs: List[String],
    referenceSourceRoots: List[File],
    subprojects: List[EnsimeModule],
    formattingPrefs: FormattingPreferences,
    sourceMode: Boolean,
    debugArgs: List[String],
    javaLibs: List[File]
) {
  (rootDir :: cacheDir :: javaHome :: referenceSourceRoots ::: javaLibs).foreach { f =>
    require(f.exists, "" + f + " is required but does not exist")
  }

  /* Proposed alternatives to the legacy wire format field names */
  def root = rootDir
  def debugVMArgs = debugArgs
  def referenceSourceJars = referenceSourceRoots

  // some marshalling libs (e.g. spray-json) might not like extra vals
  val modules = subprojects.map { module => (module.name, module) }.toMap

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
  } ++ javaLibs

  def allDocJars: Set[File] = modules.values.flatMap(_.docJars).toSet
}

case class EnsimeModule(
    name: String,
    // TODO: deprecate target/testTarget
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
    referenceSourceRoots: List[File]
) {
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

}

