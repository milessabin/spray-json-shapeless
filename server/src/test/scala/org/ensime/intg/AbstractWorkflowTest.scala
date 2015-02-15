package org.ensime.intg

import org.apache.commons.io.{ FileUtils => IOFileUtils }

import java.io.{ File => JFile }

import akka.event.slf4j.SLF4JLogging
import org.apache.commons.io.filefilter.TrueFileFilter
import org.ensime.config.EnsimeConfig
import org.ensime.core.{ Project, AnalyzerReadyEvent, FullTypeCheckCompleteEvent, IndexerReadyEvent }
import org.ensime.intg.IntgUtil._
import org.ensime.server.Server
import org.ensime.util.TestUtil
import org.scalatest._
import pimpathon.file._
import scala.reflect.io.{ File => SFile }

import scala.concurrent.duration._
import scala.io.Source
import scala.reflect.io.Path

trait AbstractWorkflowTest extends SuiteMixin with SLF4JLogging { this: Suite =>

  def path: String

  private var projectBase: JFile = null
  private var server: Server = null

  protected var config: EnsimeConfig = null
  protected var project: Project = null
  protected var asyncHelper: AsyncMsgHelper = null

  abstract override def withFixture(test: NoArgTest) = {
    IntgUtil.withTestProject(path) { (config, project, asyncHelper) =>
      this.config = config
      this.project = project
      this.asyncHelper = asyncHelper
      super.withFixture(test) // To be stackable, must call super.withFixture
    }
  }

  private def cleanup: Unit = {
    //    log.info("Cleaning up directory")
    //    server.shutdown()
    if (projectBase.exists())
      projectBase.deleteRecursively()
  }

  private def copyFilesEnsuringUnixLines(projectSource: String, projectBase: java.io.File): Unit = {
    val srcFiles = listFiles(projectSource)
    for (srcFile <- srcFiles) {
      val relativeSrc = Path(projectSource).relativize(srcFile).toFile
      val destFile = Path(projectBase) / relativeSrc
      destFile.parent.createDirectory()
      val writer = destFile.bufferedWriter()
      val source = Source.fromFile(srcFile.path, "UTF-8")
      try {
        source.getLines().foreach(line => {
          writer.write(line)
          writer.write("\n")
        })
      } finally {
        source.close()
        writer.close()
      }
    }
  }

  private def listFiles(srcDir: String): List[SFile] = {
    val jFiles = IOFileUtils.listFiles(
      new JFile(srcDir), TrueFileFilter.INSTANCE, TrueFileFilter.INSTANCE
    )
    import scala.collection.JavaConversions._
    jFiles.toList.map(SFile(_))
  }

}
