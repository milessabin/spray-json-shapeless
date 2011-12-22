/**
*  Copyright (c) 2010, Aemon Cannon
*  All rights reserved.
*  
*  Redistribution and use in source and binary forms, with or without
*  modification, are permitted provided that the following conditions are met:
*      * Redistributions of source code must retain the above copyright
*        notice, this list of conditions and the following disclaimer.
*      * Redistributions in binary form must reproduce the above copyright
*        notice, this list of conditions and the following disclaimer in the
*        documentation and/or other materials provided with the distribution.
*      * Neither the name of ENSIME nor the
*        names of its contributors may be used to endorse or promote products
*        derived from this software without specific prior written permission.
*  
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
*  DISCLAIMED. IN NO EVENT SHALL Aemon Cannon BE LIABLE FOR ANY
*  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


package org.ensime.config
import java.io.File
import org.ensime.util._
import org.ensime.util.FileUtils._
import org.ensime.util.RichFile._
import org.ensime.util.SExp._
import scala.actors.Actor._
import scala.collection.mutable
import scala.util.matching.Regex
import scalariform.formatter.preferences._

trait FormatHandler {

  def rootDir(): Option[String]

  def name(): Option[String]
  def pack(): Option[String]
  def version(): Option[String]

  def useMaven(): Boolean
  def useIvy(): Boolean

  def ivyRuntimeConf(): Option[String]
  def ivyCompileConf(): Option[String]
  def ivyTestConf(): Option[String]
  def ivyFile(): Option[String]

  def compileDeps(): List[String]
  def runtimeDeps(): List[String]
  def testDeps(): List[String]
  def sourceRoots(): List[String]
  def target(): Option[String]

  def formatPrefs(): Map[Symbol, Any]
  def disableIndexOnStartup(): Boolean
  def excludeFromIndex(): List[Regex]

  def extraCompilerArgs(): List[String]
  def extraBuilderArgs(): List[String]
}


trait SExpFormatHelper {

  def m: Map[KeywordAtom, SExp]

  def matchError(s:String) = {
    System.err.println("Configuration Format Error: " + s)
  }

  def getStr(name: String): Option[String] = m.get(key(name)) match {
    case Some(StringAtom(s)) => Some(s)
    case None => None
    case _ => matchError("Expecting a string value at key: " + name);None
  }
  def getInt(name: String): Option[Int] = m.get(key(name)) match {
    case Some(IntAtom(i)) => Some(i)
    case None => None
    case _ => matchError("Expecting an integer value at key: " + name);None
  }
  def getBool(name: String): Boolean = m.get(key(name)) match {
    case Some(TruthAtom()) => true
    case None => false
    case _ => matchError("Expecting a nil or t value at key: " + name);false
  }
  def getStrList(name: String): List[String] = m.get(key(name)) match {
    case Some(SExpList(items: Iterable[StringAtom])) => items.map { ea => ea.value }.toList
    case None => List()
    case _ => matchError("Expecting a list of string values at key: " + name);List()
  }
  def getRegexList(name: String): List[Regex] = m.get(key(name)) match {
    case Some(SExpList(items: Iterable[StringAtom])) => items.map { ea => ea.value.r }.toList
    case None => List()
    case _ => matchError("Expecting a list of string-encoded regexps at key: " + name);List()
  }

}

object ProjectConfig {


  class SExpFormatHandler(config: SExpList) extends FormatHandler with SExpFormatHelper {

    type KeyMap = Map[KeywordAtom, SExp]

    private def subprojects(m: KeyMap): List[SExpList] = {
      m.get(key(":subprojects")) match {
        case Some(SExpList(items)) =>
        items.flatMap {
          case lst: SExpList => Some(lst)
          case _ => None
        }.toList
        case _ => List()
      }
    }

    private def subproject(m: KeyMap, projectName: String): Option[SExpList] = {
      subprojects(m).find { ea =>
        ea.toKeywordMap.get(key(":name")) match {
          case Some(StringAtom(str)) => str == projectName
          case _ => false
        }
      }
    }

    private def activeSubprojectKeyMap(main: KeyMap): Option[KeyMap] = {
      main.get(key(":active-subproject")) match {
        case Some(StringAtom(nm)) => subproject(main, nm).map(_.toKeywordMap)
        case _ => None
      }
    }

    override val m: KeyMap = {
      val mainproj = config.toKeywordMap
      val subproj = activeSubprojectKeyMap(mainproj).getOrElse(Map())
      mainproj ++ subproj
    }





    lazy val rootDir() = new OptionalStringProp(
      ":root-dir",
      "The root directory of your project. This option should be filled in by your editor."
    )

    lazy val name() = new OptionalStringProp(
      ":name",
      "The short identifier for your project. Should be the same that you use when publishing. Will be displayed in the Emacs mode-line when connected to an ENSIME server.",
      None,
      Some(":project-name")
    )

    lazy val pack() = new OptionalStringProp(
      ":package",
      "The main package for your project. Used by ENSIME to populate the project outline view.",
      None,
      Some(":project-package")
    )

    lazy val version() = new OptionalStringProp(
      ":version",
      "The current, working version of your project."
    )

    lazy val useMaven() = new BooleanProp(
      ":use-maven",
      "Use an existing pom.xml to determine the dependencies for the project. A Maven-style directory structure is assumed."
    )

    lazy val useIvy() = new BooleanProp(
      ":use-ivy",
      "Use an existing ivy.xml to determine the dependencies for the project. A Maven-style directory structure is assumed."
    )

    lazy val ivyFile() = new OptionalStringProp(
      ":ivy-file",
      "Override the default ivy.xml location.",
      Some("filename")
    )

    lazy val ivyRuntimeConf() = new OptionalStringProp(
      ":ivy-runtime-conf",
      "Specify the names of dependency profiles to be used for runtime scenarios.",
      Some("...")
    )
    lazy val ivyCompileConf() = new OptionalStringProp(
      ":ivy-compile-conf",
      "Specify the names of dependency profiles to be used for compile scenarios.",
      Some("...")
    )
    lazy val ivyTestConf() = new OptionalStringProp(
      ":ivy-test-conf",
      "Specify the names of dependency profiles to be used for test scenarios.",
      Some("...")
    )

    lazy val compileDeps() = new StringListProp(
      ":compile-deps",
      "A list of jar files and class directories to include on the compilation classpath. No recursive expansion will be done.",
      Some("([directory or filename]*)")
    )

    lazy val runtimeDeps() = new StringListProp(
      ":runtime-deps",
      "A list of jar files and class directories to include on the runtime classpath. No recursive expansion will be done.",
      Some("([directory or filename]*)")
    )

    lazy val testDeps() = new StringListProp(
      ":test-deps",
      "A list of jar files and class directories to include on the test classpath. No recursive expansion will be done.",
      Some("([directory or filename]*)")
    )

    lazy val sourceRoots() = new StringListProp(
      ":source-roots",
      "A list of directories in which to start searching for source files.",
      Some("(directory*)")
    )

    lazy val target() = new OptionalStringProp(
      ":target",
      "The root of the class output directory.",
      Some("filename")
    )

    lazy val disableIndexOnStartup() = new BooleanProp(
      ":disable-index-on-startup",
      "Disable the classpath indexing process that happens at startup. This will speed up the loading process significantly, at the cost of breaking some functionality."
    )

    lazy val excludeFromIndex() = new StringListProp(
      ":exclude-from-index",
      """Classes that match one of the excluded regular expressions will not be added to the index. This can be used to reduce memory usage and speed up loading. For example:
      \begin{mylisting}
      \begin{verbatim}
      :exclude-from-index ("com\\\\.sun\\\\..\*" "com\\\\.apple\\\\..\*")
      \end{verbatim}
      \end{mylisting}""",
      Some("(directory*)")
    )


    def excludeFromIndex(): List[Regex] = getRegexList(":exclude-from-index")
    def extraCompilerArgs(): List[String] = getStrList(":compiler-args")
    def extraBuilderArgs(): List[String] = getStrList(":builder-args")

    def formatPrefs(): Map[Symbol, Any] = m.get(key(":formatting-prefs")) match {
      case Some(list: SExpList) => {
        list.toKeywordMap.map {
          case (KeywordAtom(key), sexp: SExp) => (Symbol(key.substring(1)), sexp.toScala)
        }
      }
      case _ => Map[Symbol, Any]()
    }


  }

  /**
  * Create a ProjectConfig instance from the given
  * SExp property list.
  */
  def fromSExp(sexp: SExpList): ProjectConfig = {
    load(new SExpFormatHandler(sexp))
  }

  def load(conf: FormatHandler): ProjectConfig = {

    val rootDir: CanonFile = conf.rootDir match {
      case Some(str) => new File(str)
      case _ => new File(".")
    }

    println("Using project root: " + rootDir)

    val sourceRoots = new mutable.HashSet[CanonFile]
    val runtimeDeps = new mutable.HashSet[CanonFile]
    val compileDeps = new mutable.HashSet[CanonFile]
    var target: Option[CanonFile] = None
    var projectName: Option[String] = None

    val externalConf = if (conf.useMaven) Some(Maven)
    else if (conf.useIvy) Some(Ivy)
    else None

    for (configurator <- externalConf) {
      configurator.getConfig(rootDir, conf) match {
        case Right(ext) => {
          projectName = ext.projectName
	  println("External Config found project name: " + ext.projectName)

          sourceRoots ++= ext.sourceRoots
	  println("External Config found source roots: " + ext.sourceRoots)

          compileDeps ++= ext.compileDepFiles
	  println("External Config found compile dependencies: " + ext.runtimeDepFiles)

          runtimeDeps ++= ext.runtimeDepFiles
	  println("External Config found runtime dependencies: " + ext.runtimeDepFiles)

          target = ext.target
	  println("External Config found target: " + ext.target)
        }
        case Left(except) => {
          System.err.println("Failed to load external project information. Reason:")
          except.printStackTrace(System.err)
        }
      }
    }

    {
      val deps = maybeFiles(conf.runtimeDeps, rootDir)
      val toInclude = expandRecursively(rootDir, deps, isValidJar )
      println("Including " + toInclude.size + " dependencies.")
      runtimeDeps ++= toInclude
    }

    {
      val deps = maybeFiles(conf.compileDeps, rootDir)
      val toInclude = expandRecursively(rootDir, deps, isValidJar )
      println("Including " + toInclude.size + " dependencies.")
      compileDeps ++= toInclude
    }

    {
      val deps = maybeFiles(conf.testDeps, rootDir)
      val toInclude = expandRecursively(rootDir, deps, isValidJar )
      println("Including " + toInclude.size + " dependencies.")
      compileDeps ++= toInclude
      runtimeDeps ++= toInclude
    }

    {
      val dirs = maybeDirs(conf.sourceRoots, rootDir)
      println("Including source roots: " + dirs.mkString(", "))
      sourceRoots ++= dirs
    }

    conf.target match {
      case Some(targetDir) => {
        target = target.orElse(maybeDir(targetDir, rootDir))
      }
      case _ =>
    }

    projectName = projectName.orElse(conf.name)

    val formatPrefs: Map[Symbol, Any] = conf.formatPrefs
    println("Using formatting preferences: " + formatPrefs)

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
      rootDir, 
      sourceRoots, 
      runtimeDeps,
      compileDeps, 
      target,
      formatPrefs,
      conf.disableIndexOnStartup,
      conf.excludeFromIndex,
      conf.extraCompilerArgs,
      conf.extraBuilderArgs
    )

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
    List(), List(), List(), None, Map(), false, List(), List(), List())

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
  val target: Option[CanonFile],
  formattingPrefsMap: Map[Symbol, Any],
  val disableIndexOnStartup: Boolean,
  val excludeFromIndex: Iterable[Regex],
  val extraCompilerArgs: Iterable[String],
  val extraBuilderArgs: Iterable[String]) {

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
      case ('indentWithTabs, value: Boolean) =>
      fp.setPreference(IndentWithTabs, value)
      case ('multilineScaladocCommentsStartOnFirstLine, value: Boolean) =>
      fp.setPreference(MultilineScaladocCommentsStartOnFirstLine, value)
      case ('preserveDanglingCloseParenthesis, value: Boolean) =>
      fp.setPreference(PreserveDanglingCloseParenthesis, value)
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
    "-verbose") ++ extraCompilerArgs

  def builderArgs = List(
    "-classpath", compilerClasspath,
    "-verbose",
    "-d", target.getOrElse(new File(root, "classes")).getPath,
    sourceFilenames.mkString(" ")) ++ extraBuilderArgs

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

