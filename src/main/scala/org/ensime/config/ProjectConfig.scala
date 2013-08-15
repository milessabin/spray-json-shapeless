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
import scala.collection.immutable.StringOps
import scala.util.matching.Regex
import scalariform.formatter.preferences._

trait FormatHandler {

  def rootDir(): Option[String]

  def name(): Option[String]
  def pack(): Option[String]
  def version(): Option[String]

  def compileDeps(): List[String]
  def runtimeDeps(): List[String]
  def testDeps(): List[String]
  def compileJars(): List[String]
  def runtimeJars(): List[String]
  def sourceRoots(): List[String]
  def referenceSourceRoots(): List[String]
  def target(): Option[String]
  def testTarget(): Option[String]

  def formatPrefs(): Map[Symbol, Any]
  def disableIndexOnStartup(): Boolean
  def disableSourceLoadOnStartup(): Boolean
  def disableScalaJarsOnClasspath(): Boolean
  def onlyIncludeInIndex(): List[Regex]
  def excludeFromIndex(): List[Regex]
  def extraCompilerArgs(): List[String]
  def extraBuilderArgs(): List[String]
  def javaCompilerArgs(): List[String]
  def javaCompilerVersion(): Option[String]
}

object ProjectConfig {

  type KeyMap = Map[KeywordAtom, SExp]

  import SExp.{ key => keyword }

  trait Prop {
    def keyName: String
    def synonymKey: Option[String]
    def apply(m: KeyMap): Any
    def key: KeywordAtom = KeywordAtom(keyName)

    def matchError(s: String) = {
      System.err.println("Configuration Format Error: " + s)
    }
    def getStr(m: KeyMap, name: String): Option[String] = m.get(keyword(name)) match {
      case Some(StringAtom(s)) => Some(s)
      case None => None
      case _ => matchError("Expecting a string value at key: " + name); None
    }
    def getInt(m: KeyMap, name: String): Option[Int] = m.get(keyword(name)) match {
      case Some(IntAtom(i)) => Some(i)
      case None => None
      case _ => matchError("Expecting an integer value at key: " + name); None
    }
    def getBool(m: KeyMap, name: String): Boolean = m.get(keyword(name)) match {
      case Some(TruthAtom()) => true
      case Some(NilAtom()) => false
      case None => false
      case _ => matchError("Expecting a nil or t value at key: " + name); false
    }
    def getStrList(m: KeyMap, name: String): List[String] = m.get(keyword(name)) match {
      case Some(SExpList(items: Iterable[StringAtom])) => items.map { ea => ea.value }.toList
      case None => List()
      case _ => matchError("Expecting a list of string values at key: " + name); List()
    }
    def getRegexList(m: KeyMap, name: String): List[Regex] = m.get(keyword(name)) match {
      case Some(SExpList(items: Iterable[StringAtom])) => items.map { ea => ea.value.r }.toList
      case None => List()
      case _ => matchError("Expecting a list of string-encoded regexps at key: " + name); List()
    }
    def getMap(m: KeyMap, name: String): Map[Symbol, Any] = m.get(keyword(name)) match {
      case Some(list: SExpList) => {
        list.toKeywordMap.map {
          case (KeywordAtom(key), sexp: SExp) => (Symbol(key.substring(1)), sexp.toScala)
        }
      }
      case _ => Map[Symbol, Any]()
    }
  }

  class OptionalStringProp(val keyName: String, val synonymKey: Option[String]) extends Prop {
    def apply(m: KeyMap): Option[String] = getStr(m, keyName).orElse(synonymKey.flatMap(getStr(m, _)))
    def defaultTypeHint: String = "a string"
  }

  class BooleanProp(val keyName: String, val synonymKey: Option[String]) extends Prop {
    def apply(m: KeyMap): Boolean = getBool(m, keyName) || synonymKey.map(getBool(m, _)).getOrElse(false)
    def defaultTypeHint: String = "[t or nil]"
  }

  class StringListProp(val keyName: String, val synonymKey: Option[String]) extends Prop {
    def apply(m: KeyMap): List[String] = getStrList(m, keyName) ++ synonymKey.map(getStrList(m, _)).getOrElse(List[String]())
    def defaultTypeHint: String = "(string*)"
  }

  class RegexListProp(val keyName: String, val synonymKey: Option[String]) extends Prop {
    def apply(m: KeyMap): List[Regex] = getRegexList(m, keyName) ++ synonymKey.map(getRegexList(m, _)).getOrElse(List[Regex]())
    def defaultTypeHint: String = "(regex*)"
  }

  class SymbolMapProp(val keyName: String, val synonymKey: Option[String]) extends Prop {
    def apply(m: KeyMap): Map[Symbol, Any] = getMap(m, keyName) ++ synonymKey.map(getMap(m, _)).getOrElse(Map[Symbol, Any]())
    def defaultTypeHint: String = "([keyword value]*)"
  }

  class SExpFormatHandler(config: SExpList) extends FormatHandler {

    private def subprojects(m: KeyMap): List[KeyMap] = {
      m.get(key(":subprojects")) match {
        case Some(SExpList(items)) =>
          items.flatMap {
            case lst: SExpList => Some(lst.toKeywordMap)
            case _ => None
          }.toList
        case _ => List()
      }
    }

    private def mergeWithDependency(main: KeyMap, dep: KeyMap): KeyMap = {

      def merge(primary: Option[SExp], secondary: Option[SExp]): Option[SExp] = {
        (primary, secondary) match {
          case (Some(s1: SExp), None) => Some(s1)
          case (None, Some(s2: SExp)) => Some(s2)
          case (Some(SExpList(items1)), Some(SExpList(items2))) =>
            Some(SExpList(items1 ++ items2))
          case (Some(s1: SExp), Some(s2: SExp)) => Some(s2)
          case _ => None
        }
      }

      def withMerged(m: KeyMap, key: KeywordAtom): KeyMap = {
        merge(m.get(key), dep.get(key)) match {
          case Some(sexp) => m + ((key, sexp))
          case None => m
        }
      }

      List(
        sourceRoots_.key,
        runtimeDeps_.key,
        compileDeps_.key,
        testDeps_.key).foldLeft(main)(withMerged)
    }

    private def subproject(m: KeyMap, moduleName: String): Option[KeyMap] = {
      val main = subprojects(m).find { ea =>
        ea.get(moduleName_.key) match {
          case Some(StringAtom(str)) => str == moduleName
          case _ => false
        }
      }
      main.map { p: KeyMap =>
        val deps = (p.get(dependsOnModules_.key) match {
          case Some(names: SExpList) => names.map(_.toString)
          case _ => List()
        }).flatMap { subproject(m, _) }
        deps.foldLeft(p)(mergeWithDependency)
      }
    }

    private def activeSubprojectKeyMap(main: KeyMap): Option[KeyMap] = {
      main.get(activeSubproject_.key) match {
        case Some(StringAtom(nm)) => subproject(main, nm)
        case _ => None
      }
    }

    lazy val props = scala.collection.mutable.ListBuffer[Prop]()

    /**
     * Doc Property:
     *   :root-dir
     * Summary:
     *   The root directory of your project. This option should be filled in by your editor.
     * Arguments:
     *   String: a filename
     */
    lazy val rootDir_ = new OptionalStringProp(":root-dir", None)
    props += rootDir_
    def rootDir = rootDir_(m)

    /**
     * Doc Property:
     *   :name
     * Summary:
     *   The short identifier for your project. Should be the same that you use
     *     when publishing. Will be displayed in the Emacs mode-line when
     *     connected to an ENSIME server.
     * Arguments:
     *   String: name
     */
    lazy val name_ = new OptionalStringProp(":name", Some(":project-name"))
    props += name_
    def name = name_(m)

    /**
     * Doc Property:
     *   :package
     * Summary:
     *   An optional 'primary' package for your project. Used by ENSIME to populate
     *     the project outline view.
     * Arguments:
     *   String: package name
     */
    lazy val pack_ = new OptionalStringProp(":package", Some(":project-package"))
    props += pack_
    def pack = pack_(m)

    /**
     * Doc Property:
     *   :module-name
     * Summary:
     *   The canonical module-name of this project.
     * Arguments:
     *   String: name
     */
    lazy val moduleName_ = new OptionalStringProp(":module-name", None)
    props += moduleName_
    def moduleName = moduleName_(m)

    /**
     * Doc Property:
     *   :active-subproject
     * Summary:
     *   The module-name of the subproject which is currently selected.
     * Arguments:
     *   String: module name
     */
    lazy val activeSubproject_ = new OptionalStringProp(":active-subproject", None)
    props += activeSubproject_
    def activeSubproject = activeSubproject_(m)

    /**
     * Doc Property:
     *   :depends-on-modules
     * Summary:
     *   A list of module-names on which this project depends.
     * Arguments:
     *   List of Strings: module names
     */
    lazy val dependsOnModules_ = new StringListProp(":depends-on-modules", None)
    props += dependsOnModules_
    def dependsOnModules = dependsOnModules_(m)

    /**
     * Doc Property:
     *   :version
     * Summary:
     *   The current, working version of your project.
     * Arguments:
     *   String: version number
     */
    lazy val version_ = new OptionalStringProp(":version", None)
    props += version_
    def version = version_(m)

    /**
     * Doc Property:
     *   :compile-deps
     * Summary:
     *   A list of jar files and class directories to include on the compilation
     *     classpath. No recursive expansion will be done.
     * Arguments:
     *   List of Strings: file and directory names
     */
    lazy val compileDeps_ = new StringListProp(":compile-deps", None)
    props += compileDeps_
    def compileDeps = compileDeps_(m)

    /**
     * Doc Property:
     *   :compile-jars
     * Summary:
     *   A list of jar files and directories to search for jar files to include
     *     on the compilation classpath. Directories will be searched recursively.
     * Arguments:
     *   List of Strings: file and directory names
     */
    lazy val compileJars_ = new StringListProp(":compile-jars", None)
    props += compileJars_
    def compileJars = compileJars_(m)

    /**
     * Doc Property:
     *   :runtime-deps
     * Summary:
     *   A list of jar files and class directories to include on the runtime
     *     classpath. No recursive expansion will be done.
     * Arguments:
     *   List of Strings: file and directory names
     */
    lazy val runtimeDeps_ = new StringListProp(":runtime-deps", None)
    props += runtimeDeps_
    def runtimeDeps = runtimeDeps_(m)

    /**
     * Doc Property:
     *   :runtime-jars
     * Summary:
     *   A list of jar files and directories to search for jar files to include
     *     on the runtime classpath. Directories will be searched recursively.
     * Arguments:
     *   List of Strings: file and directory names
     */
    lazy val runtimeJars_ = new StringListProp(":runtime-jars", None)
    props += runtimeJars_
    def runtimeJars = runtimeJars_(m)

    /**
     * Doc Property:
     *   :test-deps
     * Summary:
     *   A list of jar files and class directories to include on the test
     *     classpath. No recursive expansion will be done.
     * Arguments:
     *   List of Strings: file and directory names
     */
    lazy val testDeps_ = new StringListProp(":test-deps", None)
    props += testDeps_
    def testDeps = testDeps_(m)

    /**
     * Doc Property:
     *   :source-roots
     * Summary:
     *   A list of directories in which to start searching for source files.
     * Arguments:
     *   List of Strings: directory names
     */
    lazy val sourceRoots_ = new StringListProp(":source-roots", Some(":sources"))
    props += sourceRoots_
    def sourceRoots = sourceRoots_(m)

    /**
     * Doc Property:
     *   :reference-source-roots
     * Summary:
     *   A list of directories in which to start searching for reference
     *   sources. Generally these are the sources that correspond to library
     *   dependencies.
     * Arguments:
     *   List of Strings: directory names
     */
    lazy val referenceSourceRoots_ = new StringListProp(":reference-source-roots", None)
    props += referenceSourceRoots_
    def referenceSourceRoots = referenceSourceRoots_(m)

    /**
     * Doc Property:
     *   :target
     * Summary:
     *   The root of the class output directory.
     * Arguments:
     *   String: directory
     */
    lazy val target_ = new OptionalStringProp(":target", None)
    props += target_
    def target = target_(m)

    /**
     * Doc Property:
     *   :target
     * Summary:
     *   The root of the test class output directory.
     * Arguments:
     *   String: directory
     */
    lazy val testTarget_ = new OptionalStringProp(":test-target", None)
    props += testTarget_
    def testTarget = testTarget_(m)

    /**
     * Doc Property:
     *   :disable-index-on-startup
     * Summary:
     *   Disable the classpath indexing process that happens at startup.
     *     This will speed up the loading process significantly, at the
     *     cost of breaking some functionality.
     * Arguments:
     *   Boolean: t or nil
     */
    lazy val disableIndexOnStartup_ = new BooleanProp(":disable-index-on-startup", None)
    props += disableIndexOnStartup_
    def disableIndexOnStartup = disableIndexOnStartup_(m)

    /**
     * Doc Property:
     *   :disable-source-load-on-startup
     * Summary:
     *   Disable the parsing and reloading of all sources that normally
     *     occurs on startup.
     * Arguments:
     *   Boolean: t or nil
     */
    lazy val disableSourceLoadOnStartup_ = new BooleanProp(":disable-source-load-on-startup", None)
    props += disableSourceLoadOnStartup_
    def disableSourceLoadOnStartup = disableSourceLoadOnStartup_(m)

    /**
     * Doc Property:
     *   :disable-scala-jars-on-classpath
     * Summary:
     *   Disable putting standard Scala jars (i.e. scala-library.jar,
     *     scala-reflect.jar and scala-compiler) on the classpath.
     *     Useful for compiling against custom Scala builds and for
     *     development of Scala compiler.
     * Arguments:
     *   Boolean: t or nil
     */
    lazy val disableScalaJarsOnClasspath_ = new BooleanProp(":disable-scala-jars-on-classpath", None)
    props += disableScalaJarsOnClasspath_
    def disableScalaJarsOnClasspath = disableScalaJarsOnClasspath_(m)

    /**
     * Doc Property:
     *   :only-include-in-index
     * Summary:
     *   Only classes that match one of the given regular expressions will be
     *     added to the index. If this is omitted, all classes will be added.
     *     This can be used to reduce memory usage and speed up loading.
     *     For example:
     *     \begin{mylisting}
     *     \begin{verbatim}:only-include-in-index ("my\\.project\\.packages\\.\*" "important\\.dependency\\..\*")\end{verbatim}
     *     \end{mylisting}
     *     This option can be used in conjunction with 'exclude-from-index' -
     *     the result when both are given is that the exclusion expressions are
     *     applied to the names that pass the inclusion filter.
     * Arguments:
     *   List of Strings: regular expresions
     */
    lazy val onlyIncludeInIndex_ = new RegexListProp(":only-include-in-index", None)
    props += onlyIncludeInIndex_
    def onlyIncludeInIndex = onlyIncludeInIndex_(m)

    /**
     * Doc Property:
     *   :exclude-from-index
     * Summary:
     *   Classes that match one of the given regular expressions will not be
     *     added to the index. This can be used to reduce memory usage and
     *     speed up loading.
     *     For example:
     *     \begin{mylisting}
     * 	   \begin{verbatim}:exclude-from-index ("com\\.sun\\..\*" "com\\.apple\\..\*")\end{verbatim}
     * 	   \end{mylisting}
     *     This option can be used in conjunction with 'only-include-in-index' -
     *     the result when both are given is that the exclusion expressions are
     *     applied to the names that pass the inclusion filter.
     * Arguments:
     *   List of Strings: regular expresions
     */
    lazy val excludeFromIndex_ = new RegexListProp(":exclude-from-index", None)
    props += excludeFromIndex_
    def excludeFromIndex = excludeFromIndex_(m)

    /**
     * Doc Property:
     *   :compiler-args
     * Summary:
     *   Specify arguments that should be passed to ENSIME's internal
     *     presentation compiler. Warning: the presentation compiler
     *     understands a subset of the batch compiler's arguments.
     * Arguments:
     *   List of Strings: arguments
     */
    lazy val extraCompilerArgs_ = new StringListProp(":compiler-args", None)
    props += extraCompilerArgs_
    def extraCompilerArgs = extraCompilerArgs_(m)

    /**
     * Doc Property:
     *   :builder-args
     * Summary:
     *   Specify arguments that should be passed to ENSIME's internal
     *     incremental compiler.
     * Arguments:
     *   List of Strings: arguments
     */
    lazy val extraBuilderArgs_ = new StringListProp(":builder-args", None)
    props += extraBuilderArgs_
    def extraBuilderArgs = extraBuilderArgs_(m)

    /**
     * Doc Property:
     *   :java-compiler-args
     * Summary:
     *   Specify arguments that should be passed to ENSIME's internal
     *   JDT java compiler. Arguments are passed as a list of strings,
     *   with each pair being a key, value drawn from
     *   org.eclipse.jdt.internal.compiler.impl.CompilerOptions
     * Arguments:
     *   List of Strings: arguments
     */
    lazy val javaCompilerArgs_ = new StringListProp(":java-compiler-args", None)
    props += javaCompilerArgs_
    def javaCompilerArgs = javaCompilerArgs_(m)

    /**
     * Doc Property:
     *   :java-compiler-version
     * Summary:
     *   Specify version of java compiler to use (must be supported by internal
     *   JDT).
     * Arguments:
     *   String: version
     */
    lazy val javaCompilerVersion_ = new OptionalStringProp(
      ":java-compiler-version", None)
    props += javaCompilerVersion_
    def javaCompilerVersion = javaCompilerVersion_(m)

    /**
     * Doc Property:
     *   :formatting-prefs
     * Summary:
     *   Customize the behavior of the source formatter. All Scalariform
     *     preferences are supported:
     * 	  \vspace{1 cm}
     * 	  \begin{tabular}{|l|l|}
     * 	  \hline
     * 	  {\bf :alignParameters} & t or nil  \\ \hline
     * 	  {\bf :alignSingleLineCaseStatements} & t or nil  \\ \hline
     * 	  {\bf :alignSingleLineCaseStatements\_maxArrowIndent} & 1-100  \\ \hline
     * 	  {\bf :compactStringConcatenation} & t or nil  \\ \hline
     * 	  {\bf :doubleIndentClassDeclaration} & t or nil  \\ \hline
     * 	  {\bf :indentLocalDefs} & t or nil  \\ \hline
     * 	  {\bf :indentPackageBlocks} & t or nil  \\ \hline
     * 	  {\bf :indentSpaces} & 1-10  \\ \hline
     * 	  {\bf :indentWithTabs} & t or nil  \\ \hline
     * 	  {\bf :multilineScaladocCommentsStartOnFirstLine} & t or nil  \\ \hline
     * 	  {\bf :preserveDanglingCloseParenthesis} & t or nil \\ \hline
     * 	  {\bf :preserveSpaceBeforeArguments} & t or nil  \\ \hline
     * 	  {\bf :rewriteArrowSymbols} & t or nil  \\ \hline
     * 	  {\bf :spaceBeforeColon} & t or nil  \\ \hline
     * 	  {\bf :spaceInsideBrackets} & t or nil  \\ \hline
     * 	  {\bf :spaceInsideParentheses} & t or nil  \\ \hline
     * 	  {\bf :spacesWithinPatternBinders} & t or nil  \\ \hline
     * 	  \end{tabular}
     * Arguments:
     *   List of keyword, string pairs: preferences
     */
    lazy val formatPrefs_ = new SymbolMapProp(":formatting-prefs", None)
    props += formatPrefs_
    def formatPrefs = formatPrefs_(m)

    private def simpleMerge(m1: KeyMap, m2: KeyMap): KeyMap = {
      val keys = Set() ++ m1.keys ++ m2.keys
      val merged: Map[KeywordAtom, SExp] = keys.map { key =>
        (m1.get(key), m2.get(key)) match {
          case (Some(s1), None) => (key, s1)
          case (None, Some(s2)) => (key, s2)
          case (Some(SExpList(items1)),
            Some(SExpList(items2))) => (key, SExpList(items1 ++ items2))
          case (Some(s1: SExp), Some(s2: SExp)) => (key, s2)
          case _ => (key, NilAtom())
        }
      }.toMap
      merged
    }

    private lazy val m: KeyMap = {
      val mainproj = config.toKeywordMap
      val subproj = activeSubprojectKeyMap(mainproj).getOrElse(Map())
      simpleMerge(mainproj, subproj)
    }

  }

  /**
   * Create a ProjectConfig instance from the given
   * SExp property list.
   */
  def fromSExp(sexp: SExp): Either[Throwable, ProjectConfig] = {
    try {
      sexp match {
        case s: SExpList => Right(load(new SExpFormatHandler(s)))
        case _ => Left(new RuntimeException("Expected a SExpList."))
      }
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        Left(e)
    }
  }

  def load(conf: FormatHandler): ProjectConfig = {

    val serverRoot = new File(".")

    val rootDir: CanonFile = conf.rootDir match {
      case Some(str) => new File(str)
      case _ => {
        System.err.println("No project root specified!")
        serverRoot
      }
    }

    println("Using project root: " + rootDir)

    val runtimeDeps = new mutable.HashSet[CanonFile]
    val compileDeps = new mutable.HashSet[CanonFile]


    {
      val deps = canonicalizeFiles(conf.compileJars, rootDir)
      val jars = expandRecursively(rootDir, deps, isValidJar)
      println("Including compile jars: " + jars.mkString(","))
      compileDeps ++= jars
      val moreDeps = canonicalizeFiles(conf.compileDeps, rootDir)
      println("Including compile deps: " + moreDeps.mkString(","))
      compileDeps ++= moreDeps
    }

    {
      val deps = canonicalizeFiles(conf.runtimeJars, rootDir)
      val jars = expandRecursively(rootDir, deps, isValidJar)
      println("Including runtime jars: " + jars.mkString(","))
      runtimeDeps ++= jars
      val moreDeps = canonicalizeFiles(conf.runtimeDeps, rootDir)
      println("Including runtime deps: " + moreDeps.mkString(","))
      runtimeDeps ++= moreDeps
    }

    {
      val moreDeps = canonicalizeFiles(conf.testDeps, rootDir)
      println("Including test deps: " + moreDeps.mkString(","))
      compileDeps ++= moreDeps
      runtimeDeps ++= moreDeps
    }

    val sourceRoots = canonicalizeDirs(conf.sourceRoots, rootDir).toSet
    println("Using source roots: " + sourceRoots)

    val referenceSourceRoots = canonicalizeDirs(conf.referenceSourceRoots, rootDir).toSet
    println("Using reference source roots: " + referenceSourceRoots)

    var target: Option[CanonFile] = ensureDirectory(conf.target, rootDir)
    println("Using target directory: " + target.getOrElse("ERROR"))

    var testTarget: Option[CanonFile] = ensureDirectory(conf.testTarget, rootDir)
    println("Using test target directory: " + testTarget.getOrElse("ERROR"))

    val formatPrefs: Map[Symbol, Any] = conf.formatPrefs
    println("Using formatting preferences: " + formatPrefs)

    new ProjectConfig(
      conf.name,
      canonicalizeFile("lib/scala-library.jar", serverRoot),
      canonicalizeFile("lib/scala-reflect.jar", serverRoot),
      canonicalizeFile("lib/scala-compiler.jar", serverRoot),
      rootDir,
      sourceRoots,
      referenceSourceRoots,
      runtimeDeps,
      compileDeps,
      target,
      testTarget,
      formatPrefs,
      conf.disableIndexOnStartup,
      conf.disableSourceLoadOnStartup,
      conf.disableScalaJarsOnClasspath,
      conf.onlyIncludeInIndex,
      conf.excludeFromIndex,
      conf.extraCompilerArgs,
      conf.extraBuilderArgs,
      conf.javaCompilerArgs,
      conf.javaCompilerVersion)
  }

  def ensureDirectory(dir: Option[String], root: File): Option[CanonFile] = {
    (dir match {
      case Some(f) => canonicalizeDir(f, root)
      case None => Some(new File(root, "target/classes"))
    }).map { f =>
      if (!f.exists) {
        f.mkdirs
      }
      f
    }
  }

  def nullConfig = new ProjectConfig(
    name = None,
    scalaLibraryJar = None,
    scalaReflectJar = None,
    scalaCompilerJar = None,
    root = new File("."),
    sourceRoots = List(),
    referenceSourceRoots = List(),
    runtimeDeps = List(),
    compileDeps = List(),
    target = None,
    testTarget = None,
    formattingPrefsMap = Map(),
    disableIndexOnStartup = false,
    disableSourceLoadOnStartup = false,
    disableScalaJarsOnClasspath = false,
    onlyIncludeInIndex = List(),
    excludeFromIndex = List(),
    extraCompilerArgs = List(),
    extraBuilderArgs = List(),
    javaCompilerArgs = List(),
    javaCompilerVersion = None)

  def getJavaHome(): Option[File] = {
    val javaHome: String = System.getProperty("java.home");
    if (javaHome == null) None
    else Some(new File(javaHome))
  }

  def detectBootClassPath() = {
    Stream(System.getProperty("sun.boot.class.path"),
      System.getProperty("vm.boot.class.path"),
      System.getProperty("org.apache.harmony.boot.class.path")).find( _ != null )
  }

  def javaBootJars: Set[CanonFile] = {
    val bootClassPath = detectBootClassPath
    bootClassPath match {
      case Some(cpText) => {
        expandRecursively(
          new File("."),
          cpText.split(File.pathSeparatorChar).map{ new File(_) },
          isValidJar)
      }
      case _ => {
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
  }
}

class ReplConfig(val classpath: String) {}

class ProjectConfig(
  val name: Option[String],
  val scalaLibraryJar: Option[CanonFile],
  val scalaReflectJar: Option[CanonFile],
  val scalaCompilerJar: Option[CanonFile],
  val root: CanonFile,
  val sourceRoots: Iterable[CanonFile],
  val referenceSourceRoots: Iterable[CanonFile],
  val runtimeDeps: Iterable[CanonFile],
  val compileDeps: Iterable[CanonFile],
  val target: Option[CanonFile],
  val testTarget: Option[CanonFile],
  formattingPrefsMap: Map[Symbol, Any],
  val disableIndexOnStartup: Boolean,
  val disableSourceLoadOnStartup: Boolean,
  val disableScalaJarsOnClasspath: Boolean,
  val onlyIncludeInIndex: Iterable[Regex],
  val excludeFromIndex: Iterable[Regex],
  val extraCompilerArgs: Iterable[String],
  val extraBuilderArgs: Iterable[String],
  val javaCompilerArgs: Iterable[String],
  val javaCompilerVersion: Option[String]) {

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

  def scalaJars: Set[CanonFile] = Set(
    scalaCompilerJar,
    scalaReflectJar,
    scalaLibraryJar).flatten

  def compilerClasspathFilenames: Set[String] = {
    val files = if (disableScalaJarsOnClasspath) compileDeps else scalaJars ++ compileDeps
    files.map(_.getPath).toSet
  }

  def allFilesOnClasspath: Set[File] = {
    ProjectConfig.javaBootJars ++ compilerClasspathFilenames.map(new File(_))
  }

  def sources: Set[CanonFile] = {
    expandRecursively(root, sourceRoots, isValidSourceFile _).toSet
  }

  def referenceSources: Set[CanonFile] = {
    expandRecursively(root, referenceSourceRoots, isValidSourceFile _).toSet
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
    // Note: targets comes first so newly generated
    // classes will shadow classes in jars.
    val deps = target ++ testTarget ++ scalaJars ++ runtimeDeps
    val paths = deps.map(_.getPath).toSet
    paths.mkString(File.pathSeparator)
  }

  def sourcepath = {
    sourceRoots.map(_.getPath).toSet.mkString(File.pathSeparator)
  }

  def replClasspath = runtimeClasspath

  def debugClasspath = runtimeClasspath

  def replConfig = new ReplConfig(replClasspath)

}

