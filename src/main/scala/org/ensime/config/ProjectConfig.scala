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
  def compileJars(): List[String]
  def runtimeJars(): List[String]
  def sourceRoots(): List[String]
  def target(): Option[String]

  def formatPrefs(): Map[Symbol, Any]
  def disableIndexOnStartup(): Boolean
  def onlyIncludeInIndex(): List[Regex]
  def excludeFromIndex(): List[Regex]
  def extraCompilerArgs(): List[String]
  def extraBuilderArgs(): List[String]
}


object ProjectConfig {

  type KeyMap = Map[KeywordAtom, SExp]

  import SExp.{key => keyword}


  trait Prop {
    def m:KeyMap
    def key:String
    def desc:String
    def typeHint:Option[String]
    def synonymKey:Option[String]
    def apply():Any

    def defaultTypeHint:String = "NA"

    def manualEntry:String = {
      val tpe = typeHint.getOrElse(defaultTypeHint)
      List(
	"""\noindent""",
	"""{\bf """ + key + "}  " + tpe + """\\""",
	desc,
	"""\vspace{1 cm}""").mkString("\n")
    }

    def matchError(s:String) = {
      System.err.println("Configuration Format Error: " + s)
    }
    def getStr(name: String): Option[String] = m.get(keyword(name)) match {
      case Some(StringAtom(s)) => Some(s)
      case None => None
      case _ => matchError("Expecting a string value at key: " + name);None
    }
    def getInt(name: String): Option[Int] = m.get(keyword(name)) match {
      case Some(IntAtom(i)) => Some(i)
      case None => None
      case _ => matchError("Expecting an integer value at key: " + name);None
    }
    def getBool(name: String): Boolean = m.get(keyword(name)) match {
      case Some(TruthAtom()) => true
      case None => false
      case _ => matchError("Expecting a nil or t value at key: " + name);false
    }
    def getStrList(name: String): List[String] = m.get(keyword(name)) match {
      case Some(SExpList(items: Iterable[StringAtom])) => items.map { ea => ea.value }.toList
      case None => List()
      case _ => matchError("Expecting a list of string values at key: " + name);List()
    }
    def getRegexList(name: String): List[Regex] = m.get(keyword(name)) match {
      case Some(SExpList(items: Iterable[StringAtom])) => items.map { ea => ea.value.r }.toList
      case None => List()
      case _ => matchError("Expecting a list of string-encoded regexps at key: " + name);List()
    }
    def getMap(name: String): Map[Symbol, Any] = m.get(keyword(name)) match {
      case Some(list: SExpList) => {
	list.toKeywordMap.map {
          case (KeywordAtom(key), sexp: SExp) => (Symbol(key.substring(1)), sexp.toScala)
	}
      }
      case _ => Map[Symbol, Any]()
    }
  }

  class OptionalStringProp(val m:KeyMap, val key:String, val desc:String,
    val typeHint:Option[String] = None, val synonymKey:Option[String] = None) extends Prop{
    def apply():Option[String] = getStr(key).orElse(synonymKey.flatMap(getStr(_)))
    override def defaultTypeHint:String = "a string"
  }

  class BooleanProp(val m:KeyMap, val key:String, val desc:String,
    val typeHint:Option[String] = None, val synonymKey:Option[String] = None) extends Prop{
    def apply():Boolean = getBool(key) || synonymKey.map(getBool(_)).getOrElse(false)
    override def defaultTypeHint:String = "[t or nil]"
  }

  class StringListProp(val m:KeyMap, val key:String, val desc:String,
    val typeHint:Option[String] = None, val synonymKey:Option[String] = None) extends Prop{
    def apply():List[String] = getStrList(key) ++ synonymKey.map(getStrList(_)).getOrElse(List[String]())
    override def defaultTypeHint:String = "(string*)"
  }

  class RegexListProp(val m:KeyMap, val key:String, val desc:String,
    val typeHint:Option[String] = None, val synonymKey:Option[String] = None) extends Prop{
    def apply():List[Regex] = getRegexList(key) ++ synonymKey.map(getRegexList(_)).getOrElse(List[Regex]())
    override def defaultTypeHint:String = "(regex*)"
  }

  class SymbolMapProp(val m:KeyMap, val key:String, val desc:String,
    val typeHint:Option[String] = None, val synonymKey:Option[String] = None) extends Prop{
    def apply():Map[Symbol, Any] = getMap(key) ++ synonymKey.map(getMap(_)).getOrElse(Map[Symbol,Any]())
    override def defaultTypeHint:String = "([keyword value]*)"
  }

  class SExpFormatHandler(config: SExpList) extends FormatHandler {


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

    private val m: KeyMap = {
      val mainproj = config.toKeywordMap
      val subproj = activeSubprojectKeyMap(mainproj).getOrElse(Map())
      mainproj ++ subproj
    }

    val props = scala.collection.mutable.ListBuffer[Prop]()

    val rootDir_ = new OptionalStringProp(
      m,
      ":root-dir",
      "The root directory of your project. This option should be filled in by your editor.",
      Some("a filename")
    )
    props += rootDir_
    def rootDir() = rootDir_()



    val name_ = new OptionalStringProp(
      m,
      ":name",
      "The short identifier for your project. Should be the same that you use when publishing. Will be displayed in the Emacs mode-line when connected to an ENSIME server.",
      None,
      Some(":project-name")
    )
    props += name_
    def name() = name_()



    val pack_ = new OptionalStringProp(
      m,
      ":package",
      "The main package for your project. Used by ENSIME to populate the project outline view.",
      None,
      Some(":project-package")
    )
    props += pack_
    def pack() = pack_()



    val version_ = new OptionalStringProp(
      m,
      ":version",
      "The current, working version of your project."
    )
    props += version_
    def version() = version_()



    val useMaven_ = new BooleanProp(
      m,
      ":use-maven",
      "Use an existing pom.xml to determine the dependencies for the project. A Maven-style directory structure is assumed."
    )
    props += useMaven_
    def useMaven() = useMaven_()



    val useIvy_ = new BooleanProp(
      m,
      ":use-ivy",
      "Use an existing ivy.xml to determine the dependencies for the project. A Maven-style directory structure is assumed."
    )
    props += useIvy_
    def useIvy() = useIvy_()




    val ivyFile_ = new OptionalStringProp(
      m,
      ":ivy-file",
      "Override the default ivy.xml location.",
      Some("a filename")
    )
    props += ivyFile_
    def ivyFile() = ivyFile_()



    val ivyRuntimeConf_ = new OptionalStringProp(
      m,
      ":ivy-runtime-conf",
      "Specify the names of dependency profiles to be used for runtime scenarios."
    )
    props += ivyRuntimeConf_
    def ivyRuntimeConf() = ivyRuntimeConf_()



    val ivyCompileConf_ = new OptionalStringProp(
      m,
      ":ivy-compile-conf",
      "Specify the names of dependency profiles to be used for compile scenarios."
    )
    props += ivyCompileConf_
    def ivyCompileConf() = ivyCompileConf_()




    val ivyTestConf_ = new OptionalStringProp(
      m,
      ":ivy-test-conf",
      "Specify the names of dependency profiles to be used for test scenarios."
    )
    props += ivyTestConf_
    def ivyTestConf() = ivyTestConf_()



    val compileDeps_ = new StringListProp(
      m,
      ":compile-deps",
      "A list of jar files and class directories to include on the compilation classpath. No recursive expansion will be done.",
      Some("([directory or filename]*)")
    )
    props += compileDeps_
    def compileDeps() = compileDeps_()



    val compileJars_ = new StringListProp(
      m,
      ":compile-jars",
      "A list of jar files and directories to search for jar files to include on the compilation classpath.",
      Some("([directory or filename]*)")
    )
    props += compileJars_
    def compileJars() = compileJars_()



    val runtimeDeps_ = new StringListProp(
      m,
      ":runtime-deps",
      "A list of jar files and class directories to include on the runtime classpath. No recursive expansion will be done.",
      Some("([directory or filename]*)")
    )
    props += runtimeDeps_
    def runtimeDeps() = runtimeDeps_()


    val runtimeJars_ = new StringListProp(
      m,
      ":runtime-jars",
      "A list of jar files and directories to search for jar files to include on the runtime classpath.",
      Some("([directory or filename]*)")
    )
    props += runtimeJars_
    def runtimeJars() = runtimeJars_()


    val testDeps_ = new StringListProp(
      m,
      ":test-deps",
      "A list of jar files and class directories to include on the test classpath. No recursive expansion will be done.",
      Some("([directory or filename]*)")
    )
    props += testDeps_
    def testDeps() = testDeps_()


    val sourceRoots_ = new StringListProp(
      m,
      ":source-roots",
      "A list of directories in which to start searching for source files.",
      Some("(directory*)")
    )
    props += sourceRoots_
    def sourceRoots() = sourceRoots_()



    val target_ = new OptionalStringProp(
      m,
      ":target",
      "The root of the class output directory.",
      Some("filename")
    )
    props += target_
    def target() = target_()



    val disableIndexOnStartup_ = new BooleanProp(
      m,
      ":disable-index-on-startup",
      "Disable the classpath indexing process that happens at startup. This will speed up the loading process significantly, at the cost of breaking some functionality."
    )
    props += disableIndexOnStartup_
    def disableIndexOnStartup() = disableIndexOnStartup_()


    val onlyIncludeInIndex_ = new RegexListProp(
      m,
      ":only-include-in-index",
      """Only classes that match one of the given regular expressions will be added to the index. If this is omitted, all classes will be added. This can be used to reduce memory usage and speed up loading. For example:
      \begin{mylisting}
      \begin{verbatim}:only-include-in-index ("my\\.project\\.packages\\.\*" "important\\.dependency\\..\*")\end{verbatim}
      \end{mylisting}
      This option can be used in conjunction with 'exclude-from-index' - the result when both are given is that the exclusion expressions are applied to the names that pass the inclusion filter."""
    )
    props += onlyIncludeInIndex_
    def onlyIncludeInIndex() = onlyIncludeInIndex_()


    val excludeFromIndex_ = new RegexListProp(
      m,
      ":exclude-from-index",
      """Classes that match one of the excluded regular expressions will not be added to the index. This can be used to reduce memory usage and speed up loading. For example:
      \begin{mylisting}
      \begin{verbatim}:exclude-from-index ("com\\.sun\\..\*" "com\\.apple\\..\*")\end{verbatim}
      \end{mylisting}
      This option can be used in conjunction with 'only-include-in-index' - the result when both are given is that the exclusion expressions are applied to the classes that pass the inclusion filter.
"""
    )
    props += excludeFromIndex_
    def excludeFromIndex() = excludeFromIndex_()


    val extraCompilerArgs_ = new StringListProp(
      m,
      ":compiler-args",
      """Specify arguments that should be passed to ENSIME's internal compiler. For example, to enable two warnings in the compiler, you might use:
      \begin{mylisting}
      \begin{verbatim}:compiler-args ("-Ywarn-dead-code" "-Ywarn-shadowing")\end{verbatim}
      \end{mylisting}"""
    )
    props += extraCompilerArgs_
    def extraCompilerArgs() = extraCompilerArgs_()



    val extraBuilderArgs_ = new StringListProp(
      m,
      ":builder-args",
      """Specify arguments that should be passed to ENSIME's internal builder."""
    )
    props += extraBuilderArgs_
    def extraBuilderArgs() = extraBuilderArgs_()


    val formatPrefs_ = new SymbolMapProp(
      m,
      ":formatting-prefs",
      """Customize the behavior of the source formatter. All Scalariform preferences are supported:\\
      \vspace{1 cm}
      \begin{tabular}{|l|l|}
      \hline
      {\bf :alignParameters} & t or nil  \\ \hline
      {\bf :alignSingleLineCaseStatements} & t or nil  \\ \hline
      {\bf :alignSingleLineCaseStatements\_maxArrowIndent} & 1-100  \\ \hline
      {\bf :compactStringConcatenation} & t or nil  \\ \hline
      {\bf :doubleIndentClassDeclaration} & t or nil  \\ \hline
      {\bf :indentLocalDefs} & t or nil  \\ \hline
      {\bf :indentPackageBlocks} & t or nil  \\ \hline
      {\bf :indentSpaces} & 1-10  \\ \hline
      {\bf :indentWithTabs} & t or nil  \\ \hline
      {\bf :multilineScaladocCommentsStartOnFirstLine} & t or nil  \\ \hline
      {\bf :preserveDanglingCloseParenthesis} & t or nil \\ \hline
      {\bf :preserveSpaceBeforeArguments} & t or nil  \\ \hline
      {\bf :rewriteArrowSymbols} & t or nil  \\ \hline
      {\bf :spaceBeforeColon} & t or nil  \\ \hline
      {\bf :spaceInsideBrackets} & t or nil  \\ \hline
      {\bf :spaceInsideParentheses} & t or nil  \\ \hline
      {\bf :spacesWithinPatternBinders} & t or nil  \\ \hline
      \end{tabular}
      """
    )
    props += formatPrefs_
    def formatPrefs() = formatPrefs_()
  }

  def main(args:Array[String]) {
    import java.io._
    val out = new OutputStreamWriter(new FileOutputStream(args(0)))
    try {
      val o = new SExpFormatHandler(SExpList(List())){}
      for(prop <- o.props){
	out.write("\n\n")
	out.write(prop.manualEntry)
      }
    } finally {
      out.close()
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
      val deps = maybeFiles(conf.compileJars, rootDir)
      val jars = expandRecursively(rootDir, deps, isValidJar )
      println("Including compile jars: " + jars.mkString(","))
      compileDeps ++= jars
      val moreDeps = maybeFiles(conf.compileDeps, rootDir)
      println("Including compile deps: " + moreDeps.mkString(","))
      compileDeps ++= moreDeps
    }


    {
      val deps = maybeFiles(conf.runtimeJars, rootDir)
      val jars = expandRecursively(rootDir, deps, isValidJar )
      println("Including compile jars: " + jars.mkString(","))
      runtimeDeps ++= jars
      val moreDeps = maybeFiles(conf.runtimeDeps, rootDir)
      println("Including compile deps: " + moreDeps.mkString(","))
      runtimeDeps ++= moreDeps
    }


    {
      val moreDeps = maybeFiles(conf.testDeps, rootDir)
      println("Including test deps: " + moreDeps.mkString(","))
      compileDeps ++= moreDeps
      runtimeDeps ++= moreDeps
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
      conf.onlyIncludeInIndex,
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
    List(), List(), None, Map(), false, List(), List(), List(), List())

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
  val onlyIncludeInIndex: Iterable[Regex],
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
    (scalaJars ++ compileDeps).map(_.getPath).toSet
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
    val deps = scalaJars ++ runtimeDeps ++ target
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

