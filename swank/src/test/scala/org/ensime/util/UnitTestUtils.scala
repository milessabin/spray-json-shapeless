package org.ensime.util

import java.io.File

import akka.actor.ActorSystem
import org.ensime.util.RichFile._
import pimpathon.file._

import scala.concurrent.duration._

object UnitTestUtils {
  def withCanonTempDir[A](a: File => A) = withTempDirectory { dir => a(dir.canon) }

  def stringToWireString(s: String) =
    "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  def fileToWireString(file: File) = stringToWireString(file.canon.getAbsolutePath)

  // DO NOT USE deprecating breaks the build, prefer TestKit
  def withActorSystem[T](f: ActorSystem => T): T = {
    val system = ActorSystem("withActorSystem")
    try {
      f(system)
    } finally {
      system.shutdown()
      system.awaitTermination(10 seconds)
    }
  }
}
