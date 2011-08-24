package org.ensime.config
import expectj.{ExpectJ, Spawn, Executor}
import java.io.File
import java.util.regex.Pattern
import scala.util.matching._
import org.ensime.util._
import scala.collection.mutable.ArrayBuffer

case class SbtSubproject(name: String, deps: List[String])

object Sbt extends ExternalConfigurator {

  class IndexTracker(var i: Int)
  private implicit object IndexTracker extends IndexTracker(0)

  private def mostRecentStr(implicit x: IndexTracker, shell: Spawn): String = {
    val all = shell.getCurrentStandardOutContents()
    val s = all.substring(x.i)
    x.i = all.length
    s
  }

  private object sbt7{

    val delim = "%%ENSIME%%"
    def expandDelim = delim.map("\"" + _ + "\"").mkString(" + ")
    def isolated(str: String) = expandDelim + " + " + str + " + " + expandDelim
    def printIsolated(str: String) = "println(" + isolated(str) + ")\n"
    val pattern: Pattern = Pattern.compile(delim + "(.+?)" + delim)
    val prompt: String = "scala> "

    def parseValues(input: String): Option[String] = {
      val m = pattern.matcher(input);
      if (m.find()) Some(m.group(1))
      else None
    }

    def eval(expr: String)(implicit shell: Spawn): String = {
      shell.send(printIsolated(expr))
      shell.expect(prompt)
      parseValues(mostRecentStr) match {
	case Some(s) => s
	case _ => throw new RuntimeException("Failed to parse result of " + expr)
      }
    }

    def evalUnit(expr: String)(implicit shell: Spawn): Unit = {
      shell.send(expr + "\n")
      shell.expect(prompt)
    }

    def evalList(expr: String)(implicit shell: Spawn): List[String] = {
      shell.send(printIsolated(expr))
      shell.expect(prompt)
      parseValues(mostRecentStr) match {
	case Some(s) if (s.startsWith("List(") && s.endsWith(")")) => {
          (s.substring(5, s.length - 1)).split(", ").toList
	}
	case _ => throw new RuntimeException("Failed to parse result of " + expr)
      }
    }

    def spawn(baseDir: File): Spawn = {
      val expectinator = new ExpectJ()
      val pathToSbtJar = (new File(".", "bin/sbt-launch-0.7.7.jar")).getCanonicalPath()
      expectinator.spawn(new Executor(){
	  def execute():Process = {
	    val pb = new ProcessBuilder("java", "-Dsbt.log.noformat=true", "-jar", pathToSbtJar, "console-project")
	    pb.directory(baseDir)
	    pb.start()
	  }
	  override def toString():String = "ENSIME-controlled sbt7 process"
	})
    }

  }


  private object sbt10{
    val singleLineSetting: Pattern = Pattern.compile("^\\[info\\] (.+)$", Pattern.MULTILINE)
    val prompt: String = "> "

    def parseSettingStr(input: String): Option[String] = {
      val m = singleLineSetting.matcher(input);
      var result: Option[String] = None
      while (m.find()) {
	result = Some(m.group(1))
      }
      result
    }

    def evalNoop()(implicit shell: Spawn): Unit = {
      shell.send("noop\n")
      shell.expect(prompt)
      mostRecentStr
    }

    def evalUnit(expr: String)(implicit shell: Spawn): Unit = {
      shell.send(expr + "\n")
      shell.expect(prompt)
      mostRecentStr
    }

    def showSetting(expr: String)(implicit shell: Spawn): String = {
      shell.send("show " + expr + "\n")
      shell.expect(prompt)
      parseSettingStr(mostRecentStr) match {
	case Some(s) => s
	case _ => throw new RuntimeException("Failed to parse result of " + expr)
      }
    }

    import scala.util.parsing.input._
    import scala.util.parsing.combinator._
    object ListParser extends RegexParsers {
      def listOpen = regex(new Regex("List\\("))
      def listClose = regex(new Regex("\\)"))
      def attrOpen = regex(new Regex("Attributed\\("))
      def attrClose = regex(new Regex("\\)"))
      def list = listOpen ~> repsep(file, ", ") <~ listClose
      def file = (attrFile | unAttrFile)
      def attrFile = attrOpen ~> unAttrFile <~ attrClose
      def unAttrFile = regex(new Regex("[^\\),]+"))
    }

    def parseAttributedFilesList(s: String):List[String] = {
      val result: ListParser.ParseResult[List[String]] = ListParser.list(
	new CharSequenceReader(s))
      result match {
	case ListParser.Success(value, next) => value
	case ListParser.Failure(errMsg, next) => {
	  System.err.println(errMsg)
	  List()
	}
	case ListParser.Error(errMsg, next) => {
	  System.err.println(errMsg)
	  List()
	}
      }
    }

    def spawn(baseDir: File): Spawn = {
      val expectinator = new ExpectJ()
      val pathToSbtJar = (new File(".", "bin/sbt-launch-0.10.1.jar")).getCanonicalPath()
      expectinator.spawn(new Executor(){
	  def execute():Process = {
	    val pb = new ProcessBuilder("java", "-Dsbt.log.noformat=true", "-jar", pathToSbtJar)
	    pb.directory(baseDir)
	    pb.start()
	  }
	  override def toString():String = "ENSIME-controlled sbt10 process"
	})
    }


  }

  def getConfig(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig] = {
    val props = JavaProperties.load(new File(baseDir, "project/build.properties"))
    props.get("sbt.version") match {
      case Some(v:String) => {
	if (v.startsWith("0.10")) getConfig10(baseDir, conf)
	else if(v.startsWith("0.7")) getConfig7(baseDir, conf)
	else Left(new RuntimeException("Unrecognized sbt version: " + v))
      }
      case None => getConfig10(baseDir, conf)
    }
  }

  private def getConfig10(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig] = {
    import sbt10._
    implicit val shell = spawn(baseDir)

    shell.expect(prompt)

    conf.sbtActiveSubproject match {
      case Some(sub) => {
	evalUnit("project " + sub.name)
      }
      case None =>
    }

    evalNoop()

    val name = showSetting("name")
    val org = showSetting("organization")
    val projectVersion = showSetting("version")
    val buildScalaVersion = showSetting("scala-version")

    val compileDeps = parseAttributedFilesList(showSetting("compile:dependency-classpath"))
    val testDeps = (
      //      parseAttributedFilesList(showSetting("test:external-dependency-classpath")) ++ 
      parseAttributedFilesList(showSetting("test:managed-classpath")) ++ 
      parseAttributedFilesList(showSetting("test:unmanaged-classpath"))
    )
    val runtimeDeps = (
      //      parseAttributedFilesList(showSetting("runtime:external-dependency-classpath")) ++ 
      parseAttributedFilesList(showSetting("runtime:managed-classpath")) ++ 
      parseAttributedFilesList(showSetting("runtime:unmanaged-classpath"))
    )
    val sourceRoots = parseAttributedFilesList(showSetting("source-directories"))
    val target = CanonFile(showSetting("class-directory"))

    shell.send("exit\n")
    shell.expectClose()
    shell.stop()

    import FileUtils._

    val compileDepFiles = maybeFiles(compileDeps, baseDir)
    val testDepFiles = maybeFiles(testDeps, baseDir)
    val runtimeDepFiles = maybeFiles(runtimeDeps, baseDir)
    val sourceRootFiles = maybeDirs(sourceRoots, baseDir)

    Right(ExternalConfig(Some(name), sourceRootFiles,
	runtimeDepFiles, compileDepFiles, testDepFiles,
	Some(target)))
  }

  private def getConfig7(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig] = {
    import sbt7._
    try {

      // ExpectJ object with a timeout of 5s
      implicit val shell = spawn(baseDir)

      shell.expect(prompt)

      conf.sbtActiveSubproject match {
	case Some(sub) => {
          evalUnit("val p = subProjects.values.find(_.name == \"" + sub.name + "\").get.asInstanceOf[DefaultProject]")
          // Fail fast if subproject was not found...
          eval("p.name")
	}
	case None => {
          evalUnit("val p = current")
	}
      }

      val name = eval("p.name")
      val org = eval("p.organization")
      val sbtVersion = eval("sbtVersion.value")
      val projectVersion = eval("p.version")
      val buildScalaVersion = eval("p.buildScalaVersion")
      val compileDeps = evalList("(p.compileClasspath +++ p.fullClasspath(Configurations.Test)).get.toList.map(_.toString)")
      val testDeps = evalList("(p.testClasspath).get.toList.map(_.toString)")
      val runtimeDeps = evalList("(p.runClasspath  +++ p.fullClasspath(Configurations.Test)).get.toList.map(_.toString)")
      val sourceRoots = evalList("(p.mainSourceRoots +++ p.testSourceRoots).get.toList.map(_.toString)")
      val target = eval("p.outputPath.toString")

      shell.send(":q\n")
      shell.expectClose()
      shell.stop()

      import FileUtils._
      val compileDepFiles = maybeFiles(compileDeps, baseDir)
      val testDepFiles = maybeFiles(testDeps, baseDir)
      val runtimeDepFiles = maybeFiles(runtimeDeps, baseDir)
      val sourceRootFiles = maybeDirs(sourceRoots, baseDir)
      val f = new File(baseDir, target)
      val targetDir = if (f.exists) { Some(toCanonFile(f)) } else { None }

      Right(ExternalConfig(Some(name), sourceRootFiles,
          runtimeDepFiles, compileDepFiles, testDepFiles,
          targetDir))

    } catch {
      case e: expectj.TimeoutException => Left(e)
      case e: Exception => Left(e)
    }

  }

  def main(args: Array[String]) {
    //    println(getConfig(new File("."), None))
  }

}
