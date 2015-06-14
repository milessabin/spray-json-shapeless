import java.io._

import com.typesafe.sbt.SbtScalariform._
import sbt.Keys._
import sbt.{IntegrationTest => It, _}
//import scoverage.ScoverageSbtPlugin.ScoverageKeys

import scala.util.{Properties, Try}

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
  val isEmacs = sys.env.get("TERM") == Some("dumb")

  // WORKAROUND https://github.com/daniel-trinh/sbt-scalariform/issues/4
  def scalariformSettingsWithIt: Seq[Setting[_]] =
    defaultScalariformSettings ++ inConfig(It)(configScalariformSettings) ++ List(
      compileInputs in (Compile, compile) <<= (compileInputs in (Compile, compile)) dependsOn (ScalariformKeys.format in Compile),
      compileInputs in (Test, compile) <<= (compileInputs in (Test, compile)) dependsOn (ScalariformKeys.format in Test),
      compileInputs in (It, compile) <<= (compileInputs in (It, compile)) dependsOn (ScalariformKeys.format in It)
    )

  // e.g. YOURKIT_AGENT=/opt/yourkit/bin/linux-x86-64/libyjpagent.so
  val yourkitAgent = Properties.envOrNone("YOURKIT_AGENT").map { name =>
    val agent = file(name)
    require(agent.exists(), s"Yourkit agent specified ($agent) does not exist")
    Seq(s"-agentpath:${agent.getCanonicalPath}")
  }.getOrElse(Nil)

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
    parallelExecution in Test := true,
    testForkedParallel in Test := true,
    javaOptions ++= Seq("-XX:MaxPermSize=256m", "-Xmx2g", "-XX:+UseConcMarkSweepGC"),
    javaOptions in run ++= yourkitAgent,
    javaOptions in Test += "-Dlogback.configurationFile=../logback-test.xml",
    testOptions in Test ++= noColorIfEmacs,
    updateOptions := updateOptions.value.withCachedResolution(true),
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
  // common dependencies
  lazy val pimpathon = "com.github.stacycurl" %% "pimpathon-core" % "1.5.0"
  lazy val shapeless = "com.chuusai" %% "shapeless" % "2.2.2"
  lazy val logback = Seq(
    "ch.qos.logback" % "logback-classic" % "1.1.3",
    "org.slf4j" % "jul-to-slf4j" % "1.7.12",
    "org.slf4j" % "jcl-over-slf4j" % "1.7.12"
  )
  val akkaVersion = "2.3.11"

  ////////////////////////////////////////////////
  // utils
  def testLibs(scalaV: String, config: String = "test") = Seq(
    "org.scalatest" %% "scalatest" % "2.2.5" % config,
    "org.scalamock" %% "scalamock-scalatest-support" % "3.2.2" % config,
    "org.scalacheck" %% "scalacheck" % "1.12.1" % config,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion % config,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion % config,
    // workaround old deps coming from scalatest
    "org.scala-lang" % "scala-reflect" % scalaV % config,
    "org.scala-lang.modules" %% "scala-xml" % "1.0.4" % config
  ) ++ logback.map(_ % config)

  def jars(cp: Classpath): String = {
    for {
      att <- cp
      file = att.data
      if file.isFile & file.getName.endsWith(".jar")
    } yield file.getAbsolutePath
  }.mkString(",")

  // WORKAROUND: https://github.com/scalatest/scalatest/issues/511
  def noColorIfEmacs = if (isEmacs) Seq(Tests.Argument("-oWF")) else Seq(Tests.Argument("-oF"))
  ////////////////////////////////////////////////

  ////////////////////////////////////////////////
  // modules
  lazy val sexpress = Project("sexpress", file("sexpress"), settings = commonSettings) settings (
    licenses := Seq("LGPL 3.0" -> url("http://www.gnu.org/licenses/lgpl-3.0.txt")),
    libraryDependencies ++= Seq(
      // losing scala-reflect dep would be good
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      shapeless,
      "org.parboiled" %% "parboiled-scala" % "1.1.7",
      pimpathon,
      "com.google.guava" % "guava" % "18.0" % "test"
    ) ++ testLibs(scalaVersion.value)
  )

  lazy val sprayJsonShapeless = Project("spray-json-shapeless", file("spray-json-shapeless"), settings = commonSettings) settings(
    licenses := Seq("LGPL 3.0" -> url("http://www.gnu.org/licenses/lgpl-3.0.txt"))
  ) settings (
    libraryDependencies ++= Seq(
      "io.spray" %% "spray-json" % "1.3.2",
      shapeless
    ) ++ logback ++ testLibs(scalaVersion.value)
  )

  lazy val api = Project("api", file("api"), settings = commonSettings) settings (
    libraryDependencies ++= Seq(
      "org.scalariform" %% "scalariform" % "0.1.6" intransitive(),
      pimpathon
    ) ++ testLibs(scalaVersion.value)
  )

  // the JSON protocol
  lazy val jerk = Project("jerk", file("jerk"), settings = commonSettings) dependsOn (
    api,
    sprayJsonShapeless,
    sprayJsonShapeless % "test->test",
    api % "test->test" // for the test data
  ) settings (
    libraryDependencies ++= testLibs(scalaVersion.value)
  )

  // the S-Exp protocol
  lazy val swank = Project("swank", file("swank"), settings = commonSettings) dependsOn (
    api,
    api % "test->test", // for the test data
    sexpress
  ) settings (
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
    ) ++ testLibs(scalaVersion.value)
  )

  lazy val testingEmpty = Project("testingEmpty", file("testing/empty"), settings = basicSettings).settings(
    //ScoverageKeys.coverageExcludedPackages := ".*"
  )

  lazy val testingSimple = Project("testingSimple", file("testing/simple"), settings = basicSettings) settings (
    //ScoverageKeys.coverageExcludedPackages := ".*",
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.5" % "test" intransitive()
  )

  lazy val testingDebug = Project("testingDebug", file("testing/debug"), settings = basicSettings).settings(
    //ScoverageKeys.coverageExcludedPackages := ".*"
  )

  lazy val testingDocs = Project("testingDocs", file("testing/docs"), settings = basicSettings).settings(
    //ScoverageKeys.coverageExcludedPackages := ".*",
    libraryDependencies ++= Seq(
      // specifically using ForecastIOLib version 1.5.1 for javadoc 1.8 output
      "com.github.dvdme" %  "ForecastIOLib" % "1.5.1",
      "com.google.guava" % "guava" % "18.0",
      "commons-io" % "commons-io" % "2.4"
    )
  )

  lazy val server = Project("server", file("server")).dependsOn(
    api, swank, jerk,
    sexpress % "test->test",
    swank % "test->test",
    // depend on "it" dependencies in "test" or sbt adds them to the release deps!
    // https://github.com/sbt/sbt/issues/1888
    testingEmpty % "test,it",
    testingSimple % "test,it",
    testingDebug % "test,it",
    testingDocs % "test,it"
  ).configs(It).settings (
    commonSettings
  ).settings (
    inConfig(It)(Defaults.testSettings)
  ).settings (
    scalariformSettingsWithIt
  ).settings (
    // careful: parallel forks are causing weird failures
    // https://github.com/sbt/sbt/issues/1890
    parallelExecution in It := false,
    // https://github.com/sbt/sbt/issues/1891
    // this is supposed to set the number of forked JVMs, but it doesn't
    // concurrentRestrictions in Global := Seq(
    //   Tags.limit(Tags.ForkedTestGroup, 4)
    // ),
    fork in It := true,
    testForkedParallel in It := true,
    javaOptions in It += "-Dfile.encoding=UTF8", // for file cloning
    testOptions in It ++= noColorIfEmacs,
    internalDependencyClasspath in Compile += { Attributed.blank(JavaTools) },
    internalDependencyClasspath in Test += { Attributed.blank(JavaTools) },
    internalDependencyClasspath in It += { Attributed.blank(JavaTools) },
    javaOptions in It ++= Seq(
      "-Dlogback.configurationFile=../logback-it.xml"
    ),
    libraryDependencies ++= Seq(
      "com.h2database" % "h2" % "1.4.187",
      "com.typesafe.slick" %% "slick" % "2.1.0",
      "com.jolbox" % "bonecp" % "0.8.0.RELEASE",
      "org.apache.commons" % "commons-vfs2" % "2.0" intransitive(),
      // lucene 4.8+ needs Java 7: http://www.gossamer-threads.com/lists/lucene/general/225300
      "org.apache.lucene" % "lucene-core" % "4.7.2",
      "org.apache.lucene" % "lucene-analyzers-common" % "4.7.2",
      "org.ow2.asm" % "asm-commons" % "5.0.4",
      "org.ow2.asm" % "asm-util" % "5.0.4",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scalap" % scalaVersion.value,
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "org.scala-refactoring" %% "org.scala-refactoring.library" % "0.6.2",
      "commons-lang" % "commons-lang" % "2.6",
      "io.spray" %% "spray-can" % "1.3.3"
    ) ++ testLibs(scalaVersion.value, "it,test")
  )

  lazy val root = Project(id = "ensime", base = file("."), settings = commonSettings) aggregate (
    api, sexpress, sprayJsonShapeless, jerk, swank, server
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
