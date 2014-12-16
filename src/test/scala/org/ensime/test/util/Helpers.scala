package org.ensime.test.util

import java.io.File

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import org.ensime.indexer._
import org.ensime.server._
import org.ensime.test.TestUtil
import org.scalatest.exceptions.TestFailedException
import org.slf4j.LoggerFactory
import org.ensime.util.RichFile._

import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.Path
import scala.tools.nsc.Settings
import scala.tools.nsc.{ Global => nscGlobal }
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.reporters.ConsoleReporter

import pimpathon.file._
import TestUtil._

object Helpers {

  def withPresCompiler(action: (File, RichPresentationCompiler) => Any) =
    withTempDirectory { tmpRaw =>
      val tmp = tmpRaw.canon
      require(tmp.isDirectory)
      implicit val actorSystem = ActorSystem.create()

      val config = basicConfig(tmp, jars = false)

      val presCompLog = LoggerFactory.getLogger(classOf[Global])
      val settings = new Settings(presCompLog.error)
      settings.YpresentationDebug.value = presCompLog.isTraceEnabled
      settings.YpresentationVerbose.value = presCompLog.isDebugEnabled
      settings.verbose.value = presCompLog.isDebugEnabled
      //settings.usejavacp.value = true
      settings.bootclasspath.append(TestUtil.scalaLib.getAbsolutePath)
      settings.classpath.value = config.compileClasspath.mkString(File.pathSeparator)

      val reporter = new StoreReporter()
      val indexer = TestProbe()
      val parent = TestProbe()

      val resolver = new SourceResolver(config)
      val search = new SearchService(config, resolver)

      val cc = new RichPresentationCompiler(
        config, settings, reporter, parent.ref, indexer.ref, search
      )
      try {
        action(tmp, cc)
      } finally {
        cc.askShutdown()
        actorSystem.shutdown()
      }
    }

  def compileScala(paths: List[String], target: String, classPath: String): Unit = {
    val settings = new Settings
    settings.outputDirs.setSingleOutput(target)
    val reporter = new ConsoleReporter(settings)
    settings.classpath.value = classPath
    val g = new nscGlobal(settings, reporter)
    val run = new g.Run
    run.compile(paths)
  }

  // TODO: needs to be in the right place, need to get tmp dir
  def srcFile(base: File, name: String, content: String, write: Boolean = false) = {
    val src = base / mainSourcePath / name
    if (write) {
      src.create()
      src.writeBytes(content.getBytes)
    }
    new BatchSourceFile(src.getPath, content)
  }

  def contents(lines: String*) = lines.mkString("\n")

  def expectFailure(msgLines: String*)(action: () => Unit) {
    try {
      action()
      throw new IllegalStateException("Expected failure! Should not have succeeded!")
    } catch {
      case e: TestFailedException =>
      case e: Throwable => throw e
    }
  }

}
