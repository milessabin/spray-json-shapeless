package org.ensime.util

import akka.event.slf4j.SLF4JLogging
import java.io.{ File }
import org.slf4j.LoggerFactory
import scala.tools.nsc.io.Path
import org.ensime.test.TestUtil
import org.scalatest.{ Matchers, FunSpec }
import pimpathon.file._
import org.ensime.util.RichFile._

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

    // bug in upstream pimpathon
    ignore("should expand recursively with non-readable directories") {
      withTempDirectory { dirRaw =>
        val dir = dirRaw.canon
        val dir1 = new File(dir, "dir1")
        dir1.mkdir()
        val dir2 = new File(dir, "dir2")
        dir2.mkdir()

        val file1 = new File(dir1, "file1.txt")
        val file2 = new File(dir2, "file2.txt")

        file1.createNewFile()
        file2.createNewFile()

        val permSet = dir2.setReadable(false)
        val entries = FileUtils.expandRecursively(dir, List(new File("dir1"), new File("dir2")), f => f.isFile)

        if (permSet) {
          assert(entries === Set(CanonFile(file1)))
          dir2.setReadable(true, false)
        } else {
          // Windows workaround: setReadable doesn't work so make this test
          // impotent.
          assert(entries === Set(CanonFile(file1), CanonFile(file2)))
        }
      }
    }
  }
}
