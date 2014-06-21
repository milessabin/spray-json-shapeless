addSbtPlugin("org.ensime" % "ensime-sbt-cmd" % "0.1.4")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.1.6")

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.6.4")

addSbtPlugin("me.lessis" % "bintray-sbt" % "0.1.1")

// workaround https://github.com/softprops/bintray-sbt/issues/26
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.4")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.11.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

// no scoverage in scala 2.9 :-(
// https://github.com/scoverage/sbt-scoverage/issues/20

//resolvers += Classpaths.sbtPluginReleases

//addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "0.99.5.1")

// TODO https://coveralls.io

