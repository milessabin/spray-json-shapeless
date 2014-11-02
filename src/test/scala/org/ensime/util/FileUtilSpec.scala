package org.ensime.util

import akka.event.slf4j.SLF4JLogging
import org.scalatest.{ Matchers, FunSpec }
import pimpathon.file._

class FileUtilSpec extends FunSpec with Matchers with SLF4JLogging {
  describe("FileUtils") {
    it("should roundtrip reading and writing a file") {
      withTempFile { f =>
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
  }
}
