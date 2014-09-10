package org.ensime.util

import java.io.{ File }
import org.slf4j.LoggerFactory
import scala.tools.nsc.io.Path
import org.ensime.test.TestUtil
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

    val log = LoggerFactory.getLogger(this.getClass)

    it("should expand recursively with non-readable directories") {
      TestUtil.withTemporaryDirectory { dir =>
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
