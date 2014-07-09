package org.ensime.test.util

import akka.actor.ActorSystem
import akka.testkit.TestProbe

import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.StoreReporter
import scala.reflect.internal.util.BatchSourceFile
import org.ensime.server._
import org.ensime.config._
import org.scalatest.exceptions.TestFailedException

object Helpers {

  def withPresCompiler(action: RichCompilerControl => Any) = {
    implicit val actorSystem = ActorSystem.create()
    val settings = new Settings(Console.println)
    settings.embeddedDefaults[RichCompilerControl]
    val reporter = new StoreReporter()
    val indexer = TestProbe()
    val parent = TestProbe()
    val cc = new RichPresentationCompiler(
      settings, reporter, parent.ref, indexer.ref, ProjectConfig.nullConfig)
    try {
      action(cc)
    } finally {
      cc.askShutdown()
      actorSystem.shutdown()
    }
  }

  def srcFile(name: String, content: String) = new BatchSourceFile(name, content)
  def contents(lines: String*) = lines.mkString("\n")

  def expectFailure(msgLines: String*)(action: () => Unit) {
    try {
      action()
      throw new IllegalStateException("Expected failure! Should not have succeeded!")
    } catch {
      case e: TestFailedException =>
        System.err.println("\n***************************************")
        System.err.println("Expected Failure:")
        System.err.println(msgLines.mkString("\n"))
        System.err.println("***************************************\n")
      case e: Throwable => throw e
    }
  }

}
