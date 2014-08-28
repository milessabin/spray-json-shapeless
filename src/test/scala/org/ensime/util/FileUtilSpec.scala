package org.ensime.util

import java.io.File

import org.scalatest.{ Matchers, FunSpec }

object FileUtilSpec {
  def withFile(f: File => Unit): Unit = {
    val file = java.io.File.createTempFile("ensimeTest", ".txt")
    try {
      f(file)
    } finally {
      if (file.exists())
        file.delete()
    }
  }
}

class FileUtilSpec extends FunSpec with Matchers {
  import FileUtilSpec._
  describe("FileUtils") {
    it("should roundtrip reading and writing a file") {
      withFile { f =>
        val contents =
          """abcdefghijklmnopqrstuvwxyz
            |1234567890
            |()
            |{}""".stripMargin
        FileUtils.replaceFileContents(f, contents)

        val readback = FileUtils.readFile(f)
        readback match {
          case Left(e) => fail(e)
          case Right(readContents) => assert(readContents == contents)
        }
      }
    }

    it("should apply edits to a list of files") {

    }
  }
}
