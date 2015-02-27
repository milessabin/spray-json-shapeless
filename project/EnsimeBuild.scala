import java.io._
import scala.util.Try
import sbt._
import Keys._
import Package.ManifestAttributes
import com.typesafe.sbt.SbtScalariform._
import scoverage.ScoverageSbtPlugin._
import scoverage.ScoverageSbtPlugin.ScoverageKeys
//import sbtrelease.ReleasePlugin._
//import ReleaseKeys._
//import com.typesafe.sbt.pgp.PgpKeys
//import PgpKeys._

object EnsimeBuild extends Build with JdkResolver {

  val scalaV = "2.11.5"

  ////////////////////////////////////////////////
  // common
  lazy val commonSettings = scalariformSettings ++ Seq(
    organization := "org.ensime",
    scalaVersion := scalaV,
    version := "0.9.10-SNAPSHOT",
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
    javaOptions ++= Seq("-XX:MaxPermSize=256m", "-Xmx2g", "-XX:+UseConcMarkSweepGC"),
    javaOptions in Test += "-Dlogback.configurationFile=../logback-test.xml",
    // 0.13.7 introduced awesomely fast resolution caching which is
    // broken for integration testing:
    // https://github.com/sbt/sbt/issues/1868
    // and without integration testing, prefer 0.13.7-RC3
    // https://github.com/sbt/sbt/issues/1776
    //updateOptions := updateOptions.value.withCachedResolution(true),
    ScoverageKeys.coverageFailOnMinimum := true,
    licenses := Seq("BSD 3 Clause" -> url("http://opensource.org/licenses/BSD-3-Clause")),
    homepage := Some(url("http://github.com/ensime/ensime-server")),
    publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.contains("SNAP")) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    credentials += Credentials(
      "Sonatype Nexus Repository Manager", "oss.sonatype.org",
      sys.env.get("SONATYPE_USERNAME").getOrElse(""),
      sys.env.get("SONATYPE_PASSWORD").getOrElse("")
    )
  )

  ////////////////////////////////////////////////
  // utils
  def testLibs(config: String = "test") = Seq(
    "org.scalatest" %% "scalatest" % "2.2.4" % config,
    "org.scalamock" %% "scalamock-scalatest-support" % "3.2.1" % config,
    "org.scalacheck" %% "scalacheck" % "1.12.1" % config,
    "com.typesafe.akka" %% "akka-testkit" % "2.3.9" % config,
    // workaround old deps coming from scalatest
    "org.scala-lang" % "scala-reflect" % scalaV,
    "org.scala-lang.modules" %% "scala-xml" % "1.0.3"
  )

  def jars(cp: Classpath): String = {
    for {
      att <- cp
      file = att.data
      if file.isFile & file.getName.endsWith(".jar")
    } yield file.getAbsolutePath
  }.mkString(",")

  val isTravis = sys.env.get("TRAVIS_SCALA_VERSION").isDefined
  ////////////////////////////////////////////////

  ////////////////////////////////////////////////
  // modules
  lazy val sexpress = Project("sexpress", file("sexpress"), settings = commonSettings) settings (
    ScoverageKeys.coverageMinimum := 89,
    licenses := Seq("LGPL 3.0" -> url("http://www.gnu.org/licenses/lgpl-3.0.txt")),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.chuusai" %% "shapeless" % "2.0.0",
      "org.parboiled" %% "parboiled-scala" % "1.1.7",
      // perhaps drop the pimpathon dependency here in the interest of
      // minimising deps and move to server only
      "com.github.stacycurl" %% "pimpathon-core" % "1.2.0",
      "com.google.guava" % "guava" % "18.0" % "test"
    ) ++ testLibs()
  )

  lazy val api = Project("api", file("api"), settings = commonSettings) settings (
    libraryDependencies ++= Seq(
      "com.github.stacycurl" %% "pimpathon-core" % "1.2.0",
      "com.danieltrinh" %% "scalariform" % "0.1.5"
    ) ++ testLibs()
  )

  lazy val swank = Project("swank", file("swank"), settings = commonSettings) dependsOn (
    api, sexpress, sexpress % "test->test"
  ) settings (
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-slf4j" % "2.3.9",
      "ch.qos.logback" % "logback-classic" % "1.1.2",
      "org.slf4j" % "jul-to-slf4j" % "1.7.10",
      "org.slf4j" % "jcl-over-slf4j" % "1.7.10"
    ) ++ testLibs()
  )

  lazy val testingEmpty = Project("testingEmpty", file("testing/empty")) settings (
    scalaVersion := scalaV,
    publishArtifact := false
  )

  lazy val testingSimple = Project("testingSimple", file("testing/simple")) settings (
    scalaVersion := scalaV,
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test" intransitive(),
    publishArtifact := false
  )

  lazy val testingDebug = Project("testingDebug", file("testing/debug")) settings (
    scalaVersion := scalaV,
    publishArtifact := false
  )

  lazy val server = Project("server", file("server")).dependsOn(
    api, swank,
    sexpress % "test->test",
    testingEmpty % "it", testingSimple % "it", testingDebug % "it"
  ).configs(IntegrationTest).settings(commonSettings: _*).
    settings(inConfig(IntegrationTest)(Defaults.testSettings): _*).settings(
      scalariformSettingsWithIt: _*
    ).settings (
    parallelExecution in Test := !isTravis,
    parallelExecution in IntegrationTest := !isTravis,
    testForkedParallel in Test := !isTravis,
    testForkedParallel in IntegrationTest := !isTravis,
    ScoverageKeys.coverageMinimum in Test := 33,
    ScoverageKeys.coverageMinimum in IntegrationTest := 70,
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
      //"com.google.guava" % "guava" % "18.0" % "test,it",
      "commons-io" % "commons-io" % "2.4" % "test,it"
    ) ++ testLibs("it,test")
  )

  // would be nice not to have to define the 'root'
  lazy val root = Project(id = "parent", base = file("."), settings = commonSettings) aggregate (
    api, sexpress, swank, server
  ) dependsOn (server) settings (
    publishArtifact := false
  )
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
