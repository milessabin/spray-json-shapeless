package org.ensime.test

import org.apache.commons.io.FileUtils.copyDirectory
import java.io.File
import java.io.IOException
import org.ensime.util.CanonFile
import org.scalatest.Tag
import scala.util.Properties
import scala.util.Properties._
import pimpathon.file._
import org.ensime.config._
import org.ensime.util.FileUtils._
import org.ensime.util.RichFile._
import org.ensime.util.FileUtils.jdkDir

object TestUtil {

  object SlowTest extends Tag("SlowTest")
  object NotOnTravis extends Tag("NotOnTravis")

  private def parseTestProp(prop: String): Set[File] =
    propOrEmpty(prop).split(",").toSet.map(file).map(_.canon)

  val compileJars = parseTestProp("ensime.compile.jars").toList
  val testJars = (parseTestProp("ensime.test.jars") -- compileJars).toList
  val mainSourcePath = "src/main/scala"
  val testSourcePath = "src/test/scala"
  val compileClassDirs = (parseTestProp("ensime.compile.classDirs")).head
  val testClassDirs = parseTestProp("ensime.test.classDirs").head
  val scalaVersion = propOrEmpty("scala.version")
  val sourceJars = parseTestProp("ensime.jars.sources").toList
  val javaSource = {
    val f = jdkDir / "src.zip"
    if (f.exists) Some(f)
    else None
  }
  val scalaLib = compileJars.find(_.getName.contains("scala-library")).get

  def stringToWireString(s: String) =
    "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  def fileToWireString(file: File) = stringToWireString(file.canon.getAbsolutePath)

  // generates an empty single module project in a temporary directory
  // and returns the config, containing many of the same settings
  // as the ensime-server project itself (source/dependency jars),
  // with options to copy ENSIME's own sources/classes into the structure.
  def basicConfig(
    tmp: File = tempDir(),
    sources: Boolean = false,
    testSources: Boolean = false,
    classes: Boolean = false,
    testClasses: Boolean = false,
    jars: Boolean = true): EnsimeConfig = {
    val base = tmp.canon
    require(base.isDirectory())

    val module = {
      val classesDir = base / "target/classes"
      val testClassesDir = base / "target/classes-test"
      val mainSourcesDir = base / mainSourcePath
      val testSourcesDir = base / testSourcePath
      classesDir.mkdirs()
      testClassesDir.mkdirs()
      mainSourcesDir.mkdirs()
      testSourcesDir.mkdirs()

      if (sources)
        copyDirectory(file(mainSourcePath), mainSourcesDir)
      if (testSources)
        copyDirectory(file(testSourcePath), testSourcesDir)

      EnsimeModule(
        "single", classesDir :: Nil, testClassesDir :: Nil, Nil,
        if (jars) compileJars else List(scalaLib), Nil,
        if (jars) testJars else Nil,
        mainSourcesDir :: testSourcesDir :: Nil,
        if (jars) sourceJars else Nil
      )
    }

    if (classes)
      copyDirectory(compileClassDirs, module.targets.head)
    if (testClasses)
      copyDirectory(testClassDirs, module.testTargets.head)

    val cacheDir = base / ".ensime_cache"
    cacheDir.mkdirs()

    EnsimeConfig(
      base.canon, cacheDir.canon,
      "simple", scalaVersion, Nil, Map(module.name -> module),
      javaSource.toList
    )
  }
}

