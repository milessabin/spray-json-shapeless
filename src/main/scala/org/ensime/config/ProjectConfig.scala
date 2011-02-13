package org.ensime.config
import java.io.File
import org.ensime.util._
import org.ensime.util.FileUtils._
import org.ensime.util.RichFile._
import org.ensime.util.SExp._
import scala.actors._
import scala.actors.Actor._
import scala.collection.mutable
import scalariform.formatter.preferences._

object ProjectConfig {

  trait FormatHandler {
    def rootDir(): Option[String]
    def useSbt(): Boolean
    def useMaven(): Boolean
    def useIvy(): Boolean
    def sbtActiveSubproject(): Option[SbtSubproject]
    def ivyRuntimeConf(): Option[String]
    def ivyCompileConf(): Option[String]
    def ivyTestConf(): Option[String]
    def ivyFile(): Option[String]
    def runtimeJars(): List[String]
    def excludeRuntimeJars(): List[String]
    def compileJars(): List[String]
    def excludeCompileJars(): List[String]
    def classDirs(): List[String]
    def sources(): List[String]
    def target(): Option[String]
    def projectName(): Option[String]
    def formatPrefs(): Map[Symbol, Any]
    def disableIndexOnStartup(): Boolean
  }

  class SExpFormatHandler(config: SExpList) extends FormatHandler {
    val m = config.toKeywordMap
    private def getStr(name: String): Option[String] = m.get(key(name)) match {
      case Some(StringAtom(s)) => Some(s)
      case _ => None
    }
    private def getInt(name: String): Option[Int] = m.get(key(name)) match {
      case Some(IntAtom(i)) => Some(i)
      case _ => None
    }
    private def getBool(name: String): Boolean = m.get(key(name)) match {
      case Some(TruthAtom()) => true
      case _ => false
    }
    private def getStrList(name: String): List[String] = m.get(key(name)) match {
      case Some(SExpList(items: Iterable[StringAtom])) => items.map { ea => ea.value }.toList
      case _ => List()
    }
    def rootDir(): Option[String] = getStr(":root-dir")
    def useSbt(): Boolean = getBool(":use-sbt")
    def useMaven(): Boolean = getBool(":use-maven")
    def useIvy(): Boolean = getBool(":use-ivy")
    private def sbtSubprojects: List[Map[KeywordAtom, SExp]] = {
      m.get(key(":sbt-subprojects")) match {
        case Some(SExpList(items)) =>
        items.flatMap {
          case lst: SExpList => Some(lst.toKeywordMap)
          case _ => None
        }.toList
        case _ => List()
      }
    }
    private def sbtSubproject(projectName: String): Option[SbtSubproject] = {
      val proj = sbtSubprojects.find { ea =>
        ea.get(key(":name")) match {
          case Some(StringAtom(str)) => str == projectName
          case _ => false
        }
      }
      proj match {
        case Some(p) => {
          Some(SbtSubproject(
              p.get(key(":name")).getOrElse("NA").toString,
              p.get(key(":deps")) match {
		case Some(SExpList(items)) => items.map(_.toString).toList
		case _ => List()
              }))
        }
        case _ => None
      }
    }
    def sbtActiveSubproject(): Option[SbtSubproject] = {
      getStr(":sbt-active-subproject") match {
        case Some(nm) => sbtSubproject(nm)
        case _ => None
      }
    }
    def ivyRuntimeConf(): Option[String] = getStr(":ivy-runtime-conf")
    def ivyCompileConf(): Option[String] = getStr(":ivy-compile-conf")
    def ivyTestConf(): Option[String] = getStr(":ivy-test-conf")
    def ivyFile(): Option[String] = getStr(":ivy-file")
    def runtimeJars(): List[String] = getStrList(":runtime-jars")
    def excludeRuntimeJars(): List[String] = getStrList(":exclude-runtime-jars")
    def compileJars(): List[String] = getStrList(":compile-jars")
    def excludeCompileJars(): List[String] = getStrList(":exclude-compile-jars")
    def classDirs(): List[String] = getStrList(":class-dirs")
    def sources(): List[String] = getStrList(":sources")
    def target(): Option[String] = getStr(":target")
    def projectName(): Option[String] = getStr(":project-name")
    def formatPrefs(): Map[Symbol, Any] = m.get(key(":formatting-prefs")) match {
      case Some(list: SExpList) => {
        list.toKeywordMap.map {
          case (KeywordAtom(key), sexp: SExp) => (Symbol(key.substring(1)), sexp.toScala)
        }
      }
      case _ => Map[Symbol, Any]()
    }
    def disableIndexOnStartup(): Boolean = getBool(":disable-index-on-startup")
  }

  /**
  * Create a ProjectConfig instance from the given
  * SExp property list.
  */
  def fromSExp(sexp: SExpList): ProjectConfig = {
    load(new SExpFormatHandler(sexp))
  }

  def load(conf: FormatHandler): ProjectConfig = {

    import ExternalConfigInterface._

    val rootDir: CanonFile = conf.rootDir match {
      case Some(str) => new File(str)
      case _ => new File(".")
    }

    println("Using project root: " + rootDir)

    val sourceRoots = new mutable.HashSet[CanonFile]
    val runtimeDeps = new mutable.HashSet[CanonFile]
    val compileDeps = new mutable.HashSet[CanonFile]
    val classDirs = new mutable.HashSet[CanonFile]
    var target: Option[CanonFile] = None
    var projectName: Option[String] = None

    if (conf.useSbt) {
      println("Using sbt..")
      val ext = getSbtConfig(rootDir, conf.sbtActiveSubproject)
      projectName = ext.projectName
      sourceRoots ++= ext.sourceRoots
      runtimeDeps ++= ext.runtimeDepJars
      compileDeps ++= ext.compileDepJars
      target = ext.target
    }

    if (conf.useMaven) {
      println("Using maven..")
      val ext = getMavenConfig(rootDir)
      projectName = ext.projectName
      sourceRoots ++= ext.sourceRoots
      runtimeDeps ++= ext.runtimeDepJars
      compileDeps ++= ext.compileDepJars
      target = ext.target
    }

    if (conf.useIvy) {
      println("Using ivy..")
      val ext = getIvyConfig(
        rootDir, conf.ivyFile.map { new File(_) },
        conf.ivyRuntimeConf,
        conf.ivyCompileConf,
        conf.ivyTestConf)
      sourceRoots ++= ext.sourceRoots
      runtimeDeps ++= ext.runtimeDepJars
      compileDeps ++= ext.compileDepJars
      compileDeps ++= ext.testDepJars
      target = ext.target
    }

    {
      val jarsAndDirs = maybeFiles(conf.runtimeJars, rootDir)
      val toInclude = expandRecursively(rootDir, jarsAndDirs, isValidJar _)
      println("Manually including " + toInclude.size + " run-time jars.")
      runtimeDeps ++= toInclude
    }

    {
      val jarsAndDirs = maybeFiles(conf.excludeRuntimeJars, rootDir)
      val toExclude = expandRecursively(rootDir, jarsAndDirs, isValidJar _)
      println("Manually excluding " + toExclude.size + " run-time jars.")
      runtimeDeps --= toExclude
    }

    {
      val jarsAndDirs = maybeFiles(conf.compileJars, rootDir)
      val toInclude = expandRecursively(rootDir, jarsAndDirs, isValidJar _)
      println("Manually including " + toInclude.size + " compile-time jars.")
      compileDeps ++= toInclude
    }

    {
      val jarsAndDirs = maybeFiles(conf.excludeCompileJars, rootDir)
      val toExclude = expandRecursively(rootDir, jarsAndDirs, isValidJar _)
      println("Manually excluding " + toExclude.size + " compile-time jars.")
      compileDeps --= toExclude
    }

    {
      val dirs = maybeDirs(conf.classDirs, rootDir)
      println("Manually including " + dirs.size + " class directories.")
      classDirs ++= expand(rootDir, dirs, isValidClassDir _)
    }

    {
      val dirs = maybeDirs(conf.sources, rootDir)
      println("Using source roots: " + dirs.mkString(", "))
      sourceRoots ++= dirs
    }

    conf.target match {
      case Some(targetDir) => {
        target = target.orElse(maybeDir(targetDir, rootDir))
      }
      case _ =>
    }

    projectName = projectName.orElse(conf.projectName)

    val formatPrefs: Map[Symbol, Any] = conf.formatPrefs
    println("Using formatting preferences: " + formatPrefs)

    // Provide fix for 2.8.0 backwards compatibility
    val implicitNotFoundJar = new File("lib/implicitNotFound.jar")
    assert(implicitNotFoundJar.exists, {
	System.err.println(
          "lib/implicitNotFound.jar not found! 2.8.0 compatibility may be broken.")
      })
    compileDeps += implicitNotFoundJar

    // Provide some reasonable defaults..
    target = verifyTargetDir(rootDir, target, new File(rootDir, "target/classes"))
    println("Using target directory: " + target.getOrElse("ERROR"))

    if (sourceRoots.isEmpty) {
      val f = new File("src")
      if (f.exists && f.isDirectory) {
        println("Using default source root, 'src'.")
        sourceRoots += f
      }
    }

    val scalaLibraryJar = new File("lib/scala-library.jar")
    val scalaCompilerJar = new File("lib/scala-compiler.jar")

    new ProjectConfig(
      projectName,
      scalaLibraryJar,
      scalaCompilerJar,
      rootDir, sourceRoots, runtimeDeps,
      compileDeps, classDirs, target,
      formatPrefs,
      conf.disableIndexOnStartup)

  }

  // If given target directory is not valid, use the default,
  // creating if necessary.
  def verifyTargetDir(rootDir: File, target: Option[File], defaultTarget: File): Option[CanonFile] = {
    val targetDir = target match {
      case Some(f: File) => {
        if (f.exists && f.isDirectory) { f } else { defaultTarget }
      }
      case None => defaultTarget
    }
    if (targetDir.exists) {
      Some(targetDir)
    } else {
      try {
        if (targetDir.mkdirs) Some(targetDir)
        else None
      } catch {
        case e => None
      }
    }
  }

  def nullConfig = new ProjectConfig(None, null, null, new File("."), List(),
    List(), List(), List(), None, Map(), false)

  def getJavaHome(): Option[File] = {
    val javaHome: String = System.getProperty("java.home");
    if (javaHome == null) None
    else Some(new File(javaHome))
  }

  def javaBootJars: Set[CanonFile] = {
    val javaHome = getJavaHome();
    javaHome match {
      case Some(javaHome) => {
        if (System.getProperty("os.name").startsWith("Mac")) {
          expandRecursively(
            new File("."),
            List(new File(javaHome, "../Classes")),
            isValidJar)
        } else {
          expandRecursively(
            new File("."),
            List(new File(javaHome, "lib")),
            isValidJar)
        }
      }
      case None => Set()
    }
  }
}

class ReplConfig(val classpath: String) {}

class DebugConfig(val classpath: String, val sourcepath: String) {}

class ProjectConfig(
  val name: Option[String],
  val scalaLibraryJar: CanonFile,  
  val scalaCompilerJar: CanonFile,  
  val root: CanonFile,
  val sourceRoots: Iterable[CanonFile],
  val runtimeDeps: Iterable[CanonFile],
  val compileDeps: Iterable[CanonFile],
  val classDirs: Iterable[CanonFile],
  val target: Option[CanonFile],
  formattingPrefsMap: Map[Symbol, Any],
  val disableIndexOnStartup: Boolean) {

  val formattingPrefs = formattingPrefsMap.
  foldLeft(FormattingPreferences()) { (fp, p) =>
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
      case ('preserveSpaceBeforeArguments, value: Boolean) =>
      fp.setPreference(PreserveSpaceBeforeArguments, value)
      case ('rewriteArrowSymbols, value: Boolean) =>
      fp.setPreference(RewriteArrowSymbols, value)
      case ('spaceBeforeColon, value: Boolean) =>
      fp.setPreference(SpaceBeforeColon, value)
      case (name, _) => {
        System.err.println("Oops, unrecognized formatting option: " + name)
        fp
      }
    }
  }

  def scalaJars: Set[CanonFile] = Set(scalaCompilerJar, scalaLibraryJar)

  def compilerClasspathFilenames: Set[String] = {
    (scalaJars ++ compileDeps ++ classDirs).map(_.getPath).toSet
  }

  def allFilesOnClasspath: Set[File] = {
    ProjectConfig.javaBootJars ++ compilerClasspathFilenames.map(new File(_))
  }

  def sources: Set[CanonFile] = {
    expandRecursively(root, sourceRoots, isValidSourceFile _).toSet
  }

  def sourceFilenames: Set[String] = {
    sources.map(_.getPath).toSet
  }

  def compilerArgs = List(
    "-classpath", compilerClasspath,
    "-verbose")

  def builderArgs = List(
    "-classpath", compilerClasspath,
    "-verbose",
    "-d", target.getOrElse(new File(root, "classes")).getPath,
    sourceFilenames.mkString(" "))

  def compilerClasspath: String = {
    val files = compilerClasspathFilenames
    if (files.isEmpty) {
      "."
    } else {
      compilerClasspathFilenames.mkString(File.pathSeparator)
    }
  }

  def runtimeClasspath: String = {
    val deps = scalaJars ++ runtimeDeps ++ classDirs ++ target
    val paths = deps.map(_.getPath).toSet
    paths.mkString(File.pathSeparator)
  }

  def sourcepath = {
    sourceRoots.map(_.getPath).toSet.mkString(File.pathSeparator)
  }

  def replClasspath = runtimeClasspath

  def debugClasspath = runtimeClasspath

  def replConfig = new ReplConfig(replClasspath)

  def debugConfig = new DebugConfig(debugClasspath, sourcepath)

}

