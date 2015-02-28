package org.ensime.util

import java.nio.charset.Charset
import akka.event.slf4j.SLF4JLogging
import org.scalatest._
import pimpathon.file._

class FileUtilSpec extends FunSpec with Matchers with Inside with SLF4JLogging {
  describe("FileUtils") {
    it("should roundtrip reading and writing a file") {
      val cs = Charset.forName("UTF-16")
      withTempFile { f =>
        val contents =
          """abcdefghijklmnopqrstuvwxyz
            |1234567890
            |()
            |{}
            |\u2192""".stripMargin
        FileUtils.replaceFileContents(f, contents, cs)

        inside(FileUtils.readFile(f, cs)) {
          case Right(readContents) =>
            readContents shouldBe contents
        }
      }
    }
  }
}
