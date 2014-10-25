package org.ensime.config

import akka.event.slf4j.SLF4JLogging
import java.io.File
import org.ensime.util._
import pimpathon.file._
import scala.util.Properties
import scalariform.formatter.preferences._
import FileUtils.jdkDir

case class EnsimeConfig(
    root: File,
    cacheDir: File,
    name: String,
    scalaVersion: String,
    compilerArgs: List[String],
    modules: Map[String, EnsimeModule],
    referenceSourceJars: List[File],
    formattingPrefs: FormattingPreferences = FormattingPreferences(),
    sourceMode: Boolean = false,
    debugVMArgs: List[String] = List.empty) {
  (root :: cacheDir :: referenceSourceJars).foreach { f =>
    require(f.exists, s"$f is required but does not exist")
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
    m: EnsimeModule => m.targets ++ m.testTargets
  }

  val javaLib = jdkDir / "jre/lib/rt.jar"

  def allJars: Set[File] = {
    modules.values.flatMap { m =>
      m.compileJars ::: m.testJars
    }.toSet
  } + javaLib

}

case class EnsimeModule(
    name: String,
    targets: List[File],
    testTargets: List[File],
    dependsOnNames: List[String],
    compileJars: List[File],
    debugJars: List[File],
    testJars: List[File],
    sourceRoots: List[File],
    referenceSourcesJars: List[File]) {
  (targets ::: testTargets ::: compileJars ::: debugJars :::
    testJars ::: sourceRoots ::: referenceSourcesJars) foreach { f =>
      require(f.exists, s"$f is required by $name but does not exist")
    }

  def dependencies(implicit config: EnsimeConfig): List[EnsimeModule] =
    dependsOnNames.map(config.modules)
}

object EnsimeConfig extends SLF4JLogging {
  def parse(configExp: SExp): EnsimeConfig = {
    import RichFile._
    import pimpathon.any._

    // NOTE: we use 'canon' to obtain the canonical form of the
    //       configuration files. But canon may fail to resolve if
    //       the file/directory does not exist, so we force create all
    //       directories, which is - admitedly - a weird side-effect.

    val rootMap = SExpExplorer(configExp).asMap
    val root = file(rootMap.getString(":root-dir")).canon
    require(root.isDirectory, ":root-dir must exist")

    implicit class RichSExp(m: SExpMapExplorer) {
      def asDirOpt(name: String): Option[File] =
        m.getStringOpt(name).map { f =>
          file(f).rebaseIfRelative(root).tap(_.mkdirs()).canon
        }

      def asDir(name: String): File = asDirOpt(name).get

      def asDirs(name: String): List[File] =
        m.getStringListOpt(name).getOrElse(Nil).map { n =>
          file(n).rebaseIfRelative(root).tap(_.mkdirs()).canon
        }
      def asFiles(name: String): List[File] =
        m.getStringListOpt(name).getOrElse(Nil).map { n =>
          file(n).rebaseIfRelative(root).canon
        }
    }

    val cacheDir = rootMap.asDir(":cache-dir")
    val name = rootMap.getString(":name")
    val scalaVersion = rootMap.getString(":scala-version")
    val compilerArgs = rootMap.getStringListOpt(":compiler-args").getOrElse(Nil)
    val referenceSourceJars = rootMap.asFiles(":reference-source-roots")
    val subProjectsSExps = rootMap.getList(":subprojects").map(_.asMap)
    val subModules = subProjectsSExps.map { entry =>
      val moduleName = entry.getString(":name")
      val targets = entry.asDirOpt(":target") match {
        case Some(target) => target :: Nil
        case None => entry.asDirs(":targets")
      }
      val testTargets = entry.asDirOpt(":test-target") match {
        case Some(target) => target :: Nil
        case None => entry.asDirs(":test-targets")
      }
      val dependentModuleNames = entry.getStringListOpt(":depends-on-modules").getOrElse(Nil)
      val compileDeps = entry.asFiles(":compile-deps")
      val debugDeps = entry.asFiles(":runtime-deps")
      val testDeps = entry.asFiles(":test-deps")
      val sourceRoots = entry.asDirs(":source-roots")
      val referenceSourceJars = entry.asFiles(":reference-source-roots")

      moduleName -> EnsimeModule(
        moduleName, targets, testTargets, dependentModuleNames,
        compileDeps, debugDeps, testDeps, sourceRoots, referenceSourceJars
      )
    }.toMap

    // TODO: surely there is a cleaner way to load the formatting prefs...
    val formattingPrefs = {
      rootMap.map.get(KeywordAtom(":formatting-prefs")) match {
        case Some(list: SExpList) =>
          list.toKeywordMap.map {
            case (KeywordAtom(key), sexp: SExp) =>
              (Symbol(key.substring(1)), sexp.toScala)
          }
        case _ => Map[Symbol, Any]()
      }
    }.foldLeft(FormattingPreferences()) { (fp, p) =>
      p match {
        case ('alignParameters, value: Boolean) =>
          fp.setPreference(AlignParameters, value)
        case ('alignSingleLineCaseStatements, value: Boolean) =>
          fp.setPreference(AlignSingleLineCaseStatements, value)
        case ('alignSingleLineCaseStatements_maxArrowIndent, value: Int) =>
          fp.setPreference(AlignSingleLineCaseStatements.MaxArrowIndent, value)
        case ('compactStringConcatenation, value: Boolean) =>
          fp.setPreference(CompactStringConcatenation, value)
        case ('doubleIndentClassDeclaration, value: Boolean) =>
          fp.setPreference(DoubleIndentClassDeclaration, value)
        case ('formatXml, value: Boolean) =>
          fp.setPreference(FormatXml, value)
        case ('indentLocalDefs, value: Boolean) =>
          fp.setPreference(IndentLocalDefs, value)
        case ('indentPackageBlocks, value: Boolean) =>
          fp.setPreference(IndentPackageBlocks, value)
        case ('indentSpaces, value: Int) =>
          fp.setPreference(IndentSpaces, value)
        case ('indentWithTabs, value: Boolean) =>
          fp.setPreference(IndentWithTabs, value)
        case ('multilineScaladocCommentsStartOnFirstLine, value: Boolean) =>
          fp.setPreference(MultilineScaladocCommentsStartOnFirstLine, value)
        case ('placeScaladocAsterisksBeneathSecondAsterisk, value: Boolean) =>
          fp.setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, value)
        case ('preserveSpaceBeforeArguments, value: Boolean) =>
          fp.setPreference(PreserveSpaceBeforeArguments, value)
        case ('spaceInsideBrackets, value: Boolean) =>
          fp.setPreference(SpaceInsideBrackets, value)
        case ('spaceInsideParentheses, value: Boolean) =>
          fp.setPreference(SpaceInsideParentheses, value)
        case ('spaceBeforeColon, value: Boolean) =>
          fp.setPreference(SpaceBeforeColon, value)
        case ('spacesWithinPatternBinders, value: Boolean) =>
          fp.setPreference(SpacesWithinPatternBinders, value)
        case ('rewriteArrowSymbols, value: Boolean) =>
          fp.setPreference(RewriteArrowSymbols, value)
        case (name, _) =>
          log.warn("unrecognized formatting option: " + name)
          fp
      }
    }

    val sourceMode = rootMap.getBooleanOpt(":source-mode").getOrElse(false)
    val debugVMArgs = rootMap.getStringListOpt(":debug-args").getOrElse(Nil)
    EnsimeConfig(
      root.tap(_.mkdirs()).canon, cacheDir.tap(_.mkdirs()).canon,
      name, scalaVersion, compilerArgs, subModules,
      referenceSourceJars, formattingPrefs, sourceMode,
      debugVMArgs
    )
  }
}
