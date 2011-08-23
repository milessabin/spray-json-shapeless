package org.ensime.config
import java.io.File
import org.apache.ivy.{ core, util }
import org.apache.ivy.ant.IvyCacheTask
import org.apache.ivy.core.report.ArtifactDownloadReport
import org.apache.maven.artifact.ant._
import org.apache.tools.ant._
import org.ensime.util._
import org.ensime.util.FileUtils._
import scala.collection.mutable.ListBuffer
import java.util.Properties

case class ExternalConfig(
  val projectName: Option[String],
  val sourceRoots: Iterable[CanonFile],
  val runtimeDepJars: Iterable[CanonFile],
  val compileDepJars: Iterable[CanonFile],
  val testDepJars: Iterable[CanonFile],
  val target: Option[CanonFile]) {}

trait ExternalConfigurator {
  def getConfig(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig]
}






