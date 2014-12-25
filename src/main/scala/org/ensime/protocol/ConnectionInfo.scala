package org.ensime.protocol

class ConnectionInfo {
  val pid = None
  val serverName: String = "ENSIME-ReferenceServer"

  // Please also update changelog in SwankProtocol.scala
  val protocolVersion: String = "0.8.10"
}
