package org.ensime.util

import java.io.File

import akka.actor.ActorSystem
import pimpathon.file._

import scala.concurrent.duration._

object UnitTestUtils {
  def stringToWireString(s: String) =
    "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  def fileToWireString(file: File) = stringToWireString(file.canon.getAbsolutePath)

}
