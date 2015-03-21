import java.io._
import scala.util.Try
import sbt._
import Keys._
import com.typesafe.sbt.SbtScalariform._
import scoverage.ScoverageSbtPlugin.ScoverageKeys

object EnsimeBuild extends Build with JdkResolver {
  /*
   WARNING: When running `server/it:test` be aware that the tests may
   fail, but sbt will report success. This is a bug in sbt
   https://github.com/sbt/sbt/issues/1890
   */

  ////////////////////////////////////////////////
  // common
  lazy val basicSettings = Seq(
    organization := "org.ensime",
    scalaVersion := "2.11.6",
    version := "0.9.10-SNAPSHOT"
  )
  val isTravis = sys.env.get("TRAVIS") == Some("true") && sys.env.get("SHIPPABLE") == None
  val isEmacs = sys.env.get("TERM") == Some("dumb")

  if (isTravis)
    concurrentRestrictions in Global += Tags.limit(Tags.Test, 1)

  lazy val commonSettings = scalariformSettings ++ basicSettings ++ Seq(
    scalacOptions in Compile ++= Seq(
      // uncomment this to debug implicit resolution compilation problems
      //"-Xlog-implicits",
      "-encoding", "UTF-8", "-target:jvm-1.6", "-feature", "-deprecation",
      "-Xfatal-warnings",
      "-language:postfixOps", "-language:implicitConversions"
    ),
    javacOptions in (Compile, compile) ++= Seq(
      "-source", "1.6", "-target", "1.6", "-Xlint:all", "-Werror",
      "-Xlint:-options", "-Xlint:-path", "-Xlint:-processing"
    ),
    javacOptions in doc ++= Seq("-source", "1.6"),
    maxErrors := 1,
    fork := true,
    parallelExecution in Test := !isTravis,
    testForkedParallel in Test := !isTravis,
    javaOptions ++= Seq("-XX:MaxPermSize=256m", "-Xmx2g", "-XX:+UseConcMarkSweepGC"),
    javaOptions in Test += "-Dlogback.configurationFile=../logback-test.xml",
    testOptions in Test ++= noColorIfEmacs,
    // 0.13.7 introduced awesomely fast resolution caching which is
    // broken for integration testing:
    // https://github.com/sbt/sbt/issues/1868
    // and without integration testing, prefer 0.13.7-RC3
    // https://github.com/sbt/sbt/issues/1776
    //updateOptions := updateOptions.value.withCachedResolution(true),
    licenses := Seq("BSD 3 Clause" -> url("http://opensource.org/licenses/BSD-3-Clause")),
    homepage := Some(url("http://github.com/ensime/ensime-server")),
    publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.contains("SNAP")) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    credentials += Credentials(
      "Sonatype Nexus Repository Manager", "oss.sonatype.org",
      sys.env.getOrElse("SONATYPE_USERNAME", ""),
      sys.env.getOrElse("SONATYPE_PASSWORD", "")
    )
  )

  ////////////////////////////////////////////////
  // utils
  def testLibs(scalaV: String, config: String = "test") = Seq(
    "org.scalatest" %% "scalatest" % "2.2.4" % config,
    "org.scalamock" %% "scalamock-scalatest-support" % "3.2.1" % config,
    "org.scalacheck" %% "scalacheck" % "1.12.1" % config,
    "com.typesafe.akka" %% "akka-testkit" % "2.3.9" % config,
    // workaround old deps coming from scalatest
    "org.scala-lang" % "scala-reflect" % scalaV % config,
    "org.scala-lang.modules" %% "scala-xml" % "1.0.3" % config
  )

  def jars(cp: Classpath): String = {
    for {
      att <- cp
      file = att.data
      if file.isFile & file.getName.endsWith(".jar")
    } yield file.getAbsolutePath
  }.mkString(",")

  // WORKAROUND: https://github.com/scalatest/scalatest/issues/511
  def noColorIfEmacs = if (isEmacs) Seq(Tests.Argument("-oW")) else Nil
  ////////////////////////////////////////////////

  ////////////////////////////////////////////////
  // modules
  lazy val sexpress = Project("sexpress", file("sexpress"), settings = commonSettings) settings (
    licenses := Seq("LGPL 3.0" -> url("http://www.gnu.org/licenses/lgpl-3.0.txt")),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.chuusai" %% "shapeless" % "2.0.0",
      "org.parboiled" %% "parboiled-scala" % "1.1.7",
      // perhaps drop the pimpathon dependency here in the interest of
      // minimising deps and move to server only
      "com.github.stacycurl" %% "pimpathon-core" % "1.2.0",
      "com.google.guava" % "guava" % "18.0" % "test"
    ) ++ testLibs(scalaVersion.value)
  )

  lazy val api = Project("api", file("api"), settings = commonSettings) settings (
    libraryDependencies ++= Seq(
      "com.github.stacycurl" %% "pimpathon-core" % "1.2.0",
      "com.danieltrinh" %% "scalariform" % "0.1.5"
    ) ++ testLibs(scalaVersion.value)
  )

  lazy val swank = Project("swank", file("swank"), settings = commonSettings) dependsOn (
    api, sexpress, sexpress % "test->test"
  ) settings (
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-slf4j" % "2.3.9",
      "ch.qos.logback" % "logback-classic" % "1.1.2",
      "org.slf4j" % "jul-to-slf4j" % "1.7.10",
      "org.slf4j" % "jcl-over-slf4j" % "1.7.10"
    ) ++ testLibs(scalaVersion.value)
  )

  lazy val testingEmpty = Project("testingEmpty", file("testing/empty"), settings = basicSettings).settings(
    ScoverageKeys.coverageExcludedPackages := ".*"
  )
                          //.settings (publishArtifact := false)

  lazy val testingSimple = Project("testingSimple", file("testing/simple"), settings = basicSettings) settings (
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test" intransitive(),
    //publishArtifact := false,
    ScoverageKeys.coverageExcludedPackages := ".*"
  )

  lazy val testingDebug = Project("testingDebug", file("testing/debug"), settings = basicSettings).settings(
    ScoverageKeys.coverageExcludedPackages := ".*"
  )
                          //.settings (publishArtifact := false)

  lazy val server = Project("server", file("server")).dependsOn(
    api, swank,
    sexpress % "test->test",
    // must depend on these in "test" as well or they are added to the main dep list by sbt!
    // https://github.com/sbt/sbt/issues/1888
    testingEmpty % "test,it", testingSimple % "test,it", testingDebug % "test,it"
  ).configs(IntegrationTest).settings(commonSettings: _*).
    settings(inConfig(IntegrationTest)(Defaults.testSettings): _*).settings(
      scalariformSettingsWithIt: _*
  ).settings (
    parallelExecution in IntegrationTest := !isTravis,
    // parallel forks are causing weird failures
    // https://github.com/sbt/sbt/issues/1890
    testForkedParallel in IntegrationTest := false,
    javaOptions in IntegrationTest += "-Dfile.encoding=UTF8", // for file cloning
    testOptions in IntegrationTest ++= noColorIfEmacs,
    internalDependencyClasspath in Compile += { Attributed.blank(JavaTools) },
    internalDependencyClasspath in Test += { Attributed.blank(JavaTools) },
    internalDependencyClasspath in IntegrationTest += { Attributed.blank(JavaTools) },
    javaOptions in IntegrationTest ++= Seq(
      "-Dlogback.configurationFile=../logback-it.xml"
    ),
    libraryDependencies ++= Seq(
      // h2 1.4.183 is bad https://github.com/ensime/ensime-server/issues/717
      "com.h2database" % "h2" % "1.4.182",
      "com.typesafe.slick" %% "slick" % "2.1.0",
      "com.jolbox" % "bonecp" % "0.8.0.RELEASE",
      "org.apache.commons" % "commons-vfs2" % "2.0" intransitive(),
      // lucene 4.8+ needs Java 7: http://www.gossamer-threads.com/lists/lucene/general/225300
      "org.apache.lucene" % "lucene-core" % "4.7.2",
      "org.apache.lucene" % "lucene-analyzers-common" % "4.7.2",
      "org.ow2.asm" % "asm-commons" % "5.0.3",
      "org.ow2.asm" % "asm-util" % "5.0.3",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scalap" % scalaVersion.value,
      "com.typesafe.akka" %% "akka-actor" % "2.3.9",
      "org.scala-refactoring" %% "org.scala-refactoring.library" % "0.6.2",
      "commons-lang" % "commons-lang" % "2.6",
      "io.spray" %% "spray-can" % "1.3.2",
      // Included for testing purposes, as an example of javadoc 1.8 output.
      "com.github.dvdme" %  "ForecastIOLib" % "1.5.1"  % "test,it",
      "com.google.guava" % "guava" % "18.0" % "test,it",
      "commons-io" % "commons-io" % "2.4" % "test,it"
    ) ++ testLibs(scalaVersion.value, "it,test")
  )

  lazy val root = Project(id = "ensime", base = file("."), settings = commonSettings) aggregate (
    api, sexpress, swank, server
  ) dependsOn (server)
}

trait JdkResolver {
  // WORKAROUND: https://github.com/typelevel/scala/issues/75
  val JavaTools: File = List(
    // manual
    sys.env.get("JDK_HOME"),
    sys.env.get("JAVA_HOME"),
    // osx
    Try("/usr/libexec/java_home".!!).toOption,
    // fallback
    sys.props.get("java.home").map(new File(_).getParent),
    sys.props.get("java.home")
  ).flatten.map { n =>
    new File(n.trim + "/lib/tools.jar")
  }.filter(_.exists()).headOption.getOrElse(
    throw new FileNotFoundException(
      """Could not automatically find the JDK/lib/tools.jar.
        |You must explicitly set JDK_HOME or JAVA_HOME.""".stripMargin
    )
  )
}
