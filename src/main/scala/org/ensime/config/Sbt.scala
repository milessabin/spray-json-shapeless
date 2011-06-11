package org.ensime.config
import expectj.{ExpectJ, Spawn}
import java.io.File
import java.util.regex.Pattern
import org.ensime.util.FileUtils
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

  private val delim = "%%ENSIME%%"
  private def expandDelim = delim.map("\"" + _ + "\"").mkString(" + ")
  private def isolated(str: String) = expandDelim + " + " + str + " + " + expandDelim
  private def printIsolated(str: String) = "println(" + isolated(str) + ")\n"
  private val pattern: Pattern = Pattern.compile(delim + "(.+?)" + delim)
  private val prompt: String = "scala> "

  private def parseValues(input: String): Option[String] = {
    val buf = ArrayBuffer[String]()
    val m = pattern.matcher(input);
    if (m.find()) Some(m.group(1))
    else None
  }

  private def eval(expr: String)(implicit shell: Spawn): String = {
    shell.send(printIsolated(expr))
    shell.expect(prompt)
    parseValues(mostRecentStr) match {
      case Some(s) => s
      case _ => throw new RuntimeException("Failed to parse result of " + expr)
    }
  }

  private def evalUnit(expr: String)(implicit shell: Spawn): Unit = {
    shell.send(expr + "\n")
    shell.expect(prompt)
  }

  private def evalList(expr: String)(implicit shell: Spawn): List[String] = {
    shell.send(printIsolated(expr))
    shell.expect(prompt)
    parseValues(mostRecentStr) match {
      case Some(s) if (s.startsWith("List(") && s.endsWith(")")) => {
        (s.substring(5, s.length - 1)).split(", ").toList
      }
      case _ => throw new RuntimeException("Failed to parse result of " + expr)
    }
  }

  def getConfig(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig] = {
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
