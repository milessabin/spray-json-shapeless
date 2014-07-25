package org.ensime.test

import java.io.File
import org.ensime.util.CanonFile

object TestUtil {

  import org.ensime.util.FileUtils._

  /** The maximum number of times a unique temporary filename is attempted to be created.*/
  private val MaximumTries = 10
  /** The producer of randomness for unique name generation.*/
  private val random = new java.util.Random
  val temporaryDirectory = new File(System.getProperty("java.io.tmpdir"))

  def stringToWireString(s: String) =
    "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  def fileToWireString(file: CanonFile) = stringToWireString(file.getAbsolutePath)

  /**
   * Creates a temporary directory and provides its location to the given function.  The directory
   * is deleted after the function returns.
   */
  def withTemporaryDirectory[T](action: File => T): T = {
    val dir = createTemporaryDirectory.getCanonicalFile
    try { action(dir) }
    finally { delete(dir) }
  }

  def createTemporaryDirectory: File = createUniqueDirectory(temporaryDirectory)

  def createUniqueDirectory(baseDirectory: File): File = {
    def create(tries: Int): File = {
      if (tries > MaximumTries)
        error("Could not create temporary directory.")
      else {
        val randomName = "sbt_" + java.lang.Integer.toHexString(random.nextInt)
        val f = new File(baseDirectory, randomName)

        try { createDirectory(f); f }
        catch { case e: Exception => create(tries + 1) }
      }
    }
    create(0)
  }
}

