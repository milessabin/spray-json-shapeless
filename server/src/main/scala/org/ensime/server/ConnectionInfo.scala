package org.ensime.server

case class EnsimeImplementation(
  name: String)
case class ConnectionInfo(
  pid: Option[Int] = None,
  implementation: EnsimeImplementation = EnsimeImplementation("ENSIME"),
  // Please also update changelog in SwankProtocol.scala
  version: String = "0.8.13")
