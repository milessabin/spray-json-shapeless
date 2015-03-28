package org.ensime.util

import akka.actor.ActorSystem
import java.io.File
import org.slf4j.LoggerFactory
import pimpathon.file._
import RichFile._
import scala.concurrent.duration._

object UnitTestUtils {
  def withCanonTempDir[A](a: File => A) = withTempDirectory { dir => a(dir.canon) }

  def stringToWireString(s: String) =
    "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  def fileToWireString(file: File) = stringToWireString(file.canon.getAbsolutePath)

  // DO NOT USE deprecating breaks the build, prefer TestKit
  def withActorSystem[T](f: ActorSystem => T): T = {
    val system = ActorSystem()
    try {
      f(system)
    } finally {
      system.shutdown()
      system.awaitTermination(20 seconds)
    }
  }
}
