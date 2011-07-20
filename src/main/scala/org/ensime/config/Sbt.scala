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

  }


  private object sbt10{
    val singleLineSetting: Pattern = Pattern.compile("^[^ ]+info[^ ]+ (.+)$", Pattern.MULTILINE)
    val prompt: String = "> "

    def parseSettingStr(input: String): Option[String] = {
      val m = singleLineSetting.matcher(input);
      if (m.find()) Some(m.group(1))
      else None
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

    def evalSettingStr(expr: String)(implicit shell: Spawn): String = {
      shell.send("show " + expr + "\n")
      shell.expect(prompt)
      parseSettingStr(mostRecentStr) match {
	case Some(s) => println("found " + s); s
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

      def unAttrFile = regex(new Regex("[a-zA-z\\.\\/]+"))

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
	    val pb = new ProcessBuilder("java", "-jar", pathToSbtJar)
	    pb.directory(baseDir)
	    pb.start()
	  }
	  override def toString():String = "ENSIME-controlled sbt10 process"
	})
    }


  }

  def getConfig(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig] = {
    //    getConfig7()
    getConfig10(baseDir, conf)
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

    val name = evalSettingStr("name")
    val org = evalSettingStr("organization")
    val projectVersion = evalSettingStr("version")
    val buildScalaVersion = evalSettingStr("scala-version")

    val compileDeps = parseAttributedFilesList(evalSettingStr("compile:dependency-classpath"))
    val testDeps = (
      parseAttributedFilesList(evalSettingStr("test:external-dependency-classpath")) ++ 
      parseAttributedFilesList(evalSettingStr("test:managed-classpath")) ++ 
      parseAttributedFilesList(evalSettingStr("test:unmanaged-classpath"))
    )
    val runtimeDeps = (
      parseAttributedFilesList(evalSettingStr("runtime:external-dependency-classpath")) ++ 
      parseAttributedFilesList(evalSettingStr("runtime:managed-classpath")) ++ 
      parseAttributedFilesList(evalSettingStr("runtime:unmanaged-classpath"))
    )
    val sourceRoots = parseAttributedFilesList(evalSettingStr("source-directories"))
    val target = CanonFile(evalSettingStr("target"))

    shell.send("exit\n")
    shell.expectClose()
    shell.stop()

    import FileUtils._

    val compileDepJars = maybeFiles(compileDeps, baseDir).filter(isValidJar)
    val testDepJars = maybeFiles(testDeps, baseDir).filter(isValidJar)
    val runtimeDepJars = maybeFiles(runtimeDeps, baseDir).filter(isValidJar)
    val sourceRootFiles = maybeDirs(sourceRoots, baseDir)

    Right(ExternalConfig(Some(name), sourceRootFiles,
        runtimeDepJars, compileDepJars, testDepJars,
        Some(target)))
  }

  private def getConfig7(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig] = {
    import sbt7._
    try {

      // ExpectJ object with a timeout of 5s
      val expectinator = new ExpectJ()
      implicit val shell = expectinator.spawn("./bin/cmd-at " +
	baseDir.getCanonicalPath + " sbt console-project")
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
      val compileDepJars = maybeFiles(compileDeps, baseDir).filter(isValidJar)
      val testDepJars = maybeFiles(testDeps, baseDir).filter(isValidJar)
      val runtimeDepJars = maybeFiles(runtimeDeps, baseDir).filter(isValidJar)
      val sourceRootFiles = maybeDirs(sourceRoots, baseDir)
      val f = new File(baseDir, target)
      val targetDir = if (f.exists) { Some(toCanonFile(f)) } else { None }

      Right(ExternalConfig(Some(name), sourceRootFiles,
          runtimeDepJars, compileDepJars, testDepJars,
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
