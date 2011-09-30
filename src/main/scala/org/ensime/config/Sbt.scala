/**
*  Copyright (C) 2010 Aemon Cannon
*
*  This program is free software; you can redistribute it and/or
*  modify it under the terms of the GNU General Public License as
*  published by the Free Software Foundation; either version 2 of
*  the License, or (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public
*  License along with this program; if not, write to the Free
*  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
*  MA 02111-1307, USA.
*/

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

  private trait SbtInstance{
    def getConfig(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig]
    def versionName:String
    def jarName:String
    def appArgs:List[String] = List()

    def spawn(baseDir: File): Spawn = {
      val expectinator = new ExpectJ()
      val pathToSbtJar = (new File(".", "bin/" + jarName)).getCanonicalPath()
      println("Using sbt at " + pathToSbtJar)
      expectinator.spawn(new Executor(){
	  def execute():Process = {
	    val envOpts = Option(System getenv "SBT_OPTS") map { _ split "\\s+" } map { arr => Vector(arr: _*) } getOrElse Vector()
	    val args = envOpts ++ Vector("-Dsbt.log.noformat=true", "-jar", pathToSbtJar) ++ sbtArgs
	    val pb = new ProcessBuilder("java" +: args: _*)
	    pb.directory(baseDir)
	    pb.start()
	  }
	  override def toString():String = "ENSIME-controlled sbt-" + versionName + " process"
	})
    }
  }


  private class Sbt7Style extends SbtInstance{

    def versionName:String = "0.7"
    def jarName:String = "sbt-launch-0.7.7.jar"
    override def appArgs:List[String] = List("console-project")

    def getConfig(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig] = {
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
	val testDepFiles = maybeFiles(testDeps, baseDir)
	val compileDepFiles = maybeFiles(compileDeps, baseDir) ++ testDepFiles
	val runtimeDepFiles = maybeFiles(runtimeDeps, baseDir) ++ testDepFiles
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


    private val delim = "%%ENSIME%%"
    private def expandDelim = delim.map("\"" + _ + "\"").mkString(" + ")
    private def isolated(str: String) = expandDelim + " + " + str + " + " + expandDelim
    private def printIsolated(str: String) = "println(" + isolated(str) + ")\n"
    private val pattern: Pattern = Pattern.compile(delim + "(.+?)" + delim)
    private val prompt: String = "scala> "

    private def parseValues(input: String): Option[String] = {
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

  }


  private class Sbt10Style extends SbtInstance{

    def versionName:String = "0.10"
    def jarName:String = "sbt-launch-0.10.1.jar"

    def getConfig(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig] = {
      implicit val shell = spawn(baseDir)

      shell.expect(prompt)

      conf.sbtActiveSubproject match {
	case Some(sub) => {
	  evalUnit("project " + sub.name)
	}
	case None =>
      }

      evalNoop()

      def getList(key:String):List[String] = {
	parseAttributedFilesList(
	  showSetting(key).getOrElse("List()"))
      }

      val name = showSetting("name").getOrElse("NA")
      val org = showSetting("organization").getOrElse("NA")
      val projectVersion = showSetting("version").getOrElse("NA")
      val buildScalaVersion = showSetting("scala-version").getOrElse("2.9.0")

      val compileDeps = (
	  getList("compile:unmanaged-classpath") ++ 
	  getList("compile:managed-classpath") ++ 
	  getList("compile:internal-dependency-classpath")
      )
      val testDeps = (
	  getList("test:unmanaged-classpath") ++
	  getList("test:managed-classpath") ++ 
	  getList("test:internal-dependency-classpath")
//	  ++ getList("test:exported-products")
      )
      val runtimeDeps = (
	  getList("runtime:unmanaged-classpath") ++
	  getList("runtime:managed-classpath") ++
	  getList("runtime:internal-dependency-classpath")
//	  ++ getList("runtime:exported-products")
      )

      val sourceRoots =  (
	getList("compile:source-directories") ++
	getList("test:source-directories")
      )
      println("parsed " + sourceRoots)
      val target = CanonFile(showSetting("class-directory").getOrElse("./classes"))

      shell.send("exit\n")
      shell.expectClose()
      shell.stop()

      import FileUtils._

      val testDepFiles = maybeFiles(testDeps, baseDir)
      val compileDepFiles = maybeFiles(compileDeps, baseDir) ++ testDepFiles
      val runtimeDepFiles = maybeFiles(runtimeDeps, baseDir) ++ testDepFiles
      val sourceRootFiles = maybeDirs(sourceRoots, baseDir)

      println("files " + sourceRoots)

      Right(ExternalConfig(Some(name), sourceRootFiles,
	  runtimeDepFiles, compileDepFiles, testDepFiles,
	  Some(target)))
    }

    private val singleLineSetting: Pattern = Pattern.compile("^\\[info\\] (.+)$", Pattern.MULTILINE)
    private val prompt: String = "> "

    private def parseSettingStr(input: String): Option[String] = {
      val m = singleLineSetting.matcher(input);
      var result: Option[String] = None
      while (m.find()) {
	result = Some(m.group(1))
      }
      result
    }

    private def evalNoop()(implicit shell: Spawn): Unit = {
      shell.send("noop\n")
      shell.expect(prompt)
      mostRecentStr
    }

    private def evalUnit(expr: String)(implicit shell: Spawn): Unit = {
      shell.send(expr + "\n")
      shell.expect(prompt)
      mostRecentStr
    }

    private def showSetting(expr: String)(implicit shell: Spawn): Option[String] = {
      shell.send("show " + expr + "\n")
      shell.expect(prompt)
      parseSettingStr(mostRecentStr)
    }

    import scala.util.parsing.input._
    import scala.util.parsing.combinator._
    private object ListParser extends RegexParsers {
      def listOpen = regex("([A-z]+)\\(".r)
      def listClose = regex("\\)".r)
      def attrOpen = regex("Attributed\\(".r)
      def attrClose = regex("\\)".r)
      def list = listOpen ~> repsep(file, ", ") <~ listClose
      def file = (attrFile | unAttrFile)
      def attrFile = attrOpen ~> unAttrFile <~ attrClose
      def unAttrFile = regex("[^\\),]+".r)
    }

    private def parseAttributedFilesList(s: String):List[String] = {
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
  }


  def getConfig(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig] = {
    val props = JavaProperties.load(new File(baseDir, "project/build.properties"))
    props.get("sbt.version") match {
      case Some(v:String) => {

	if(v.startsWith("0.7")) (new Sbt7Style()).getConfig(baseDir, conf)

	else if(v.startsWith("0.9")) (new Sbt10Style(){
	    override def versionName:String = "0.9"
	  }).getConfig(baseDir, conf)

	else if(v.startsWith("0.10")) (new Sbt10Style()).getConfig(baseDir, conf)

	else if(v.startsWith("0.11")) (new Sbt10Style(){
	    override def versionName:String = "0.11"
	    override def jarName:String = "sbt-launch-0.11.0.jar"
	  }).getConfig(baseDir, conf)

	else {
	  println("Unrecognized sbt version " + v + ". Guessing sbt-10...")
	  (new Sbt10Style()).getConfig(baseDir, conf)
	}
      }
      case None => {
	println("No project/build.properties found. Guessing sbt-10...")
	(new Sbt10Style()).getConfig(baseDir, conf)
      }
    }
  }



  def main(args: Array[String]) {

//    println(sbt10.parseAttributedFilesList("List(Attributed(/home/aemon/src/misc/ensime/lib/org.scala-refactoring_2.9.0-0.3.0-SNAPSHOT.jar), Attributed(/home/aemon/src/misc/ensime/lib/critbit-0.0.4.jar), Attributed(/home/aemon/src/misc/ensime/project/boot/scala-2.9.1/lib/scala-library.jar), Attributed(/home/aemon/src/misc/ensime/project/boot/scala-2.9.1/lib/scala-compiler.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.ant/ant/jars/ant-1.8.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.ant/ant-launcher/jars/ant-launcher-1.8.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.ivy/ivy/jars/ivy-2.1.0.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-ant-tasks/jars/maven-ant-tasks-2.1.0.jar), Attributed(/home/aemon/.ivy2/cache/ant/ant/jars/ant-1.6.5.jar), Attributed(/home/aemon/.ivy2/cache/classworlds/classworlds/jars/classworlds-1.1-alpha-2.jar), Attributed(/home/aemon/.ivy2/cache/org.codehaus.plexus/plexus-container-default/jars/plexus-container-default-1.0-alpha-9-stable-1.jar), Attributed(/home/aemon/.ivy2/cache/org.codehaus.plexus/plexus-utils/jars/plexus-utils-1.5.15.jar), Attributed(/home/aemon/.ivy2/cache/org.codehaus.plexus/plexus-interpolation/jars/plexus-interpolation-1.11.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-artifact/jars/maven-artifact-2.2.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-artifact-manager/jars/maven-artifact-manager-2.2.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-repository-metadata/jars/maven-repository-metadata-2.2.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven.wagon/wagon-provider-api/jars/wagon-provider-api-1.0-beta-6.jar), Attributed(/home/aemon/.ivy2/cache/backport-util-concurrent/backport-util-concurrent/jars/backport-util-concurrent-3.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-model/jars/maven-model-2.2.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-project/jars/maven-project-2.2.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-settings/jars/maven-settings-2.2.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-profile/jars/maven-profile-2.2.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-plugin-registry/jars/maven-plugin-registry-2.2.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-error-diagnostics/jars/maven-error-diagnostics-2.2.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven.wagon/wagon-file/jars/wagon-file-1.0-beta-6.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven.wagon/wagon-http-lightweight/jars/wagon-http-lightweight-1.0-beta-6.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven.wagon/wagon-http-shared/jars/wagon-http-shared-1.0-beta-6.jar), Attributed(/home/aemon/.ivy2/cache/nekohtml/xercesMinimal/jars/xercesMinimal-1.9.6.2.jar), Attributed(/home/aemon/.ivy2/cache/nekohtml/nekohtml/jars/nekohtml-1.9.6.2.jar), Attributed(/home/aemon/.ivy2/cache/org.sonatype.tycho/org.eclipse.jdt.core/jars/org.eclipse.jdt.core-3.6.0.v_A58.jar), Attributed(/home/aemon/.ivy2/cache/org.scalariform/scalariform_2.9.0/jars/scalariform_2.9.0-0.1.0.jar), Attributed(/home/aemon/.ivy2/cache/net.sourceforge.expectj/expectj/jars/expectj-2.0.1.jar), Attributed(/home/aemon/.ivy2/cache/commons-logging/commons-logging/jars/commons-logging-1.1.1.jar), Attributed(/home/aemon/.ivy2/cache/asm/asm/jars/asm-3.2.jar), Attributed(/home/aemon/.ivy2/cache/asm/asm-commons/jars/asm-commons-3.2.jar), Attributed(/home/aemon/.ivy2/cache/asm/asm-tree/jars/asm-tree-3.2.jar))"))

//    println(sbt10.parseAttributedFilesList("List(/Users/daniel/Local/ensime_2.9.1-0.7.RC1/src/main/scala, /Users/daniel/Local/ensime_2.9.1-0.7.RC1/src/main/java, /Users/daniel/Local/ensime_2.9.1-0.7.RC1/target/scala-2.8.1.final/src_managed/main)"))

//    println(sbt10.parseAttributedFilesList("List(    /Users/daniel/Local/ensime_2.9.1-0.7.RC1/src/main/scala,     /Users/daniel/Local/ensime_2.9.1-0.7.RC1/src/main/java,    /Users/daniel/Local/ensime_2.9.1-0.7.RC1/target/scala-2.8.1.final/src_managed/main)"))


  }

}
