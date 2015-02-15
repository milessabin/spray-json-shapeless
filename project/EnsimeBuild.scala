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

  ////////////////////////////////////////////////
  // common
  lazy val commonSettings = scalariformSettings ++ Seq(
    organization := "org.ensime",
    scalaVersion := "2.11.5",
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
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.4" % "test",
      "org.scalamock" %% "scalamock-scalatest-support" % "3.2.1" % "test",
      // scalacheck 1.12.2 flags up loads of bugs in S-Express
      "org.scalacheck" %% "scalacheck" % "1.12.1" % "test",
      "ch.qos.logback" % "logback-classic" % "1.1.2",
      "org.slf4j" % "jul-to-slf4j" % "1.7.10",
      "org.slf4j" % "jcl-over-slf4j" % "1.7.10"
    ),
    maxErrors := 1,
    fork := true,
    javaOptions ++= Seq("-XX:MaxPermSize=256m", "-Xmx2g", "-XX:+UseConcMarkSweepGC"),
    // 0.13.7 introduced awesomely fast resolution caching
    updateOptions := updateOptions.value.withCachedResolution(true),
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
  def jars(cp: Classpath): String = {
    for {
      att <- cp
      file = att.data
      if file.isFile & file.getName.endsWith(".jar")
    } yield file.getAbsolutePath
  }.mkString(",")
  def classDirs(cp: Classpath): String = {
    for {
      att <- cp
      file = att.data
      if file.isDirectory
    } yield file.getAbsolutePath
  }.mkString(",")
  ////////////////////////////////////////////////

  ////////////////////////////////////////////////
  // modules
  lazy val sexpress = Project("sexpress", file("sexpress"), settings = commonSettings) settings (
    ScoverageKeys.coverageMinimum := 90,
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.chuusai" %% "shapeless" % "2.0.0",
      "org.parboiled" %% "parboiled-scala" % "1.1.7",
      // perhaps drop the pimpathon dependency here in the interest
      // of minimising deps
      "com.github.stacycurl" %% "pimpathon-core" % "1.2.0",
      "com.google.guava" % "guava" % "18.0" % "test"
    )
  )

  lazy val api = Project("api", file("api"), settings = commonSettings)

  lazy val swank = Project("swank", file("swank"), settings = commonSettings) dependsOn (
    api, sexpress
  )

  lazy val server = Project("server", file("server"), settings = commonSettings) dependsOn(
    api, swank, sexpress % "test->test"
  ) settings (
    ScoverageKeys.coverageMinimum := 70,
    internalDependencyClasspath in Compile += { Attributed.blank(JavaTools) },
    internalDependencyClasspath in Test += { Attributed.blank(JavaTools) },
    javaOptions in Test ++= Seq(
      "-XX:MaxPermSize=256m", "-Xmx4g", "-XX:+UseConcMarkSweepGC",
      "-Densime.compile.jars=" + jars((fullClasspath in Compile).value),
      "-Densime.test.jars=" + jars((fullClasspath in Test).value),
      "-Densime.compile.classDirs=" + classDirs((fullClasspath in Compile).value),
      "-Densime.test.classDirs=" + classDirs((fullClasspath in Test).value),
      "-Dscala.version=" + scalaVersion.value,
      // sorry! this puts a source/javadoc dependency on running our tests
      "-Densime.jars.sources=" + (updateClassifiers in Test).value.select(
        artifact = artifactFilter(classifier = "sources")
      ).mkString(",")
    ),
    // adds our example projects to the test compile
    unmanagedSourceDirectories in Test += baseDirectory.value / "src/testprojects/example-simple",
    unmanagedSourceDirectories in Test += baseDirectory.value / "src/testprojects/debug/src",
    libraryDependencies ++= Seq(
      // h2 1.4.183 is bad https://github.com/ensime/ensime-server/issues/717
      "com.h2database" % "h2" % "1.4.182",
      "com.typesafe.slick" %% "slick" % "2.1.0",
      "com.jolbox" % "bonecp" % "0.8.0.RELEASE",
      "org.apache.commons" % "commons-vfs2" % "2.0" intransitive (),
      // lucene 4.8+ needs Java 7: http://www.gossamer-threads.com/lists/lucene/general/225300
      "org.apache.lucene" % "lucene-core" % "4.7.2",
      "org.apache.lucene" % "lucene-analyzers-common" % "4.7.2",
      "org.ow2.asm" % "asm-commons" % "5.0.3",
      "org.ow2.asm" % "asm-util" % "5.0.3",
      "com.danieltrinh" %% "scalariform" % "0.1.5",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scalap" % scalaVersion.value,
      "com.typesafe.akka" %% "akka-actor" % "2.3.9",
      "com.typesafe.akka" %% "akka-slf4j" % "2.3.9",
      "org.scala-refactoring" %% "org.scala-refactoring.library" % "0.6.2",
      "com.typesafe.akka" %% "akka-testkit" % "2.3.9" % "test",
      "commons-io" % "commons-io" % "2.4" % "test"
    )
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
  val jdkDir: File = List(
    // manual
    sys.env.get("JDK_HOME"),
    sys.env.get("JAVA_HOME"),
    // osx
    Try("/usr/libexec/java_home".!!.trim).toOption,
    // fallback
    sys.props.get("java.home").map(new File(_).getParent),
    sys.props.get("java.home")
  ).flatten.filter { n =>
      new File(n + "/lib/tools.jar").exists
    }.headOption.map(new File(_)).getOrElse(
      throw new FileNotFoundException(
        """Could not automatically find the JDK/lib/tools.jar.
      |You must explicitly set JDK_HOME or JAVA_HOME.""".stripMargin
      )
    )

  // epic hack to get the tools.jar JDK dependency
  val JavaTools = file(jdkDir + "/lib/tools.jar")
}
