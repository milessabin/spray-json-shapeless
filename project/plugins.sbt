// ensime-sbt is needed for the integration tests
addSbtPlugin("org.ensime" % "ensime-sbt" % "0.1.5")

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "1.0.4")

// scapegoat can be installed per-user: recommended for dev
// addSbtPlugin("com.sksamuel.scapegoat" %% "sbt-scapegoat" % "0.94.5")

addSbtPlugin("org.scoverage" %% "sbt-coveralls" % "1.0.0.BETA1")

scalacOptions in Compile ++= Seq("-feature", "-deprecation")
