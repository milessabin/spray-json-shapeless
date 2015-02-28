package org.ensime.config

import akka.event.slf4j.SLF4JLogging
import java.io.File
import org.ensime.sexp._
import org.ensime.sexp.formats._
import org.ensime.util._
import pimpathon.file._
import scalariform.formatter.preferences.FormattingPreferences
import akka.event.slf4j.Logger

object EnsimeConfigProtocol {
  object Protocol extends DefaultSexpProtocol
    with OptionAltFormat
    with CanonFileFormat
    with ScalariformFormat
    with CamelCaseToDashes
  import Protocol._

  private val log = Logger(this.getClass.getName)

  private implicit val moduleFormat = SexpFormat[EnsimeModule]
  private implicit val configFormat = SexpFormat[EnsimeConfig]

  def parse(config: String): EnsimeConfig = {
    val raw = config.parseSexp.convertTo[EnsimeConfig]
    validated(raw).copy(javaLibs = inferJavaLibs(raw.javaHome))
  }

  // there are lots of JRE libs, but most people only care about
  // rt.jar --- this could be parameterised.
  private def inferJavaLibs(javaHome: File): List[File] =
    javaHome.tree.filter(_.getName == "rt.jar").toList

  def validated(c: EnsimeConfig): EnsimeConfig = c.copy(
    subprojects = c.subprojects.map(validated(_))
  )

  /*
   We use the canonical form of files/directories to keep OS X happy
   when loading S-Expressions. But the canon may fail to resolve if
   the file/directory does not exist, so we force create all required
   directories and then re-canon them, which is - admittedly - a weird
   side-effect.
   */
  private[config] def validated(m: EnsimeModule): EnsimeModule = {
    (m.targetDirs ++ m.testTargetDirs ++ m.sourceRoots).foreach { dir =>
      if (!dir.exists()) {
        log.warn(s"$dir does not exist, creating")
        dir.mkdirs()
      }
    }
    m.copy(
      target = None,
      targets = m.targetDirs.map(canonise(_)),
      testTarget = None,
      testTargets = m.testTargetDirs.map(canonise(_)),
      sourceRoots = m.sourceRoots.map(canonise(_))
    )
  }
}
