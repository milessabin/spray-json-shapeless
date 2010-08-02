package org.ensime.test.util

import scala.actors.Actor._  
import scala.tools.nsc.interactive.{Global, CompilerControl}
import scala.tools.nsc.{Settings, FatalError}
import scala.tools.nsc.reporters.{StoreReporter}
import scala.tools.nsc.util.{SourceFile, BatchSourceFile}
import org.ensime.server._
import org.ensime.config._


object Helpers{

  def withPresCompiler(action:RichCompilerControl => Any ) =  {
    val settings = new Settings(Console.println)

    //TODO: Don't hardcode this path!
    settings.processArguments(List(
	"-classpath","project/boot/scala-2.8.0/lib/scala-library.jar",
	"-verbose"), false)
    val reporter = new StoreReporter()
    val cc:RichCompilerControl = new RichPresentationCompiler(
      settings, reporter, actor{}, ProjectConfig.nullConfig)
    cc.askNewRunnerThread
    cc.askReloadAllFiles() // <- make sure typerRun is created
    action(cc)
  }

  def srcFile(name:String, content:String) = new BatchSourceFile(name, content)
  def contents(lines:String *) = lines.mkString("\n")

}
