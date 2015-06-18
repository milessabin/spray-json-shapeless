// ensime-sbt is needed for the integration tests
addSbtPlugin("org.ensime" % "ensime-sbt" % "0.1.7")

// not working on Windows https://github.com/sbt/sbt/issues/1952
//addMavenResolverPlugin

// https://github.com/sbt/sbt-scalariform/issues/20
// the version of org.scalariform will be bumped by ensime-sbt
addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.4.0")

// sbt-coveralls needs a new release
// https://github.com/scoverage/sbt-coveralls/issues/52
//addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "1.2.0")
//addSbtPlugin("org.scoverage" %% "sbt-coveralls" % "1.0.0")

scalacOptions in Compile ++= Seq("-feature", "-deprecation")

