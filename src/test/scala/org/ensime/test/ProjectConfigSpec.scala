package org.ensime.test

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.ensime.config.ProjectConfig
import org.ensime.util.{ SExpParser, SExp, CanonFile }

import TestUtil._

class ProjectConfigSpec extends FunSpec with Matchers {

  def parse(s: String): ProjectConfig = {
    ProjectConfig.fromSExp(SExpParser.read(s)) match {
      case Right(c) => c
      case Left(t) => throw t
    }
  }

  describe("ProjectConfigSpec") {

    it("should parse a simple name correctly") {
      assert(parse("""(:name "dude")""").name.get == "dude")
    }

    it("should parse a name with backslashes and quotes") {
      assert(parse("""(:name "d\\u\"d\e")""").name.get == "d\\u\"d\\e")
    }

    it("should parse name's synonym") {
      assert(parse("""(:project-name "dude")""").name.get == "dude")
    }

    it("should respect active-subproject") {
      val conf = parse("""
        |(
        |  :subprojects (
        |     (
        |     :name "Hello"
        |     :module-name "A"
        |     )
        |     (
        |     :name "Goodbye"
        |     :module-name "B"
        |     )
        |  )
        |  :active-subproject "B"
        |)""".stripMargin)
      assert(conf.name.get == "Goodbye")
    }

    it("should correctly merge dependency") {
      withTemporaryDirectory { dir =>
        val root1 = CanonFile(createUniqueDirectory(dir))
        val root2 = CanonFile(createUniqueDirectory(dir))
        val conf = parse(s"""
          |(
          |  :subprojects (
          |     (
          |     :name "Proj A"
          |     :module-name "A"
          |     :source-roots ("$root1")
          |     )
          |     (
          |     :name "Proj B"
          |     :module-name "B"
          |     :source-roots ("$root2")
          |     :depends-on-modules ("A")
          |     )
          |  )
          |  :active-subproject "B"
          |)""".stripMargin)
        assert(conf.name.get == "Proj B")

        assert(conf.sourceRoots.toSet.contains(root1))
        assert(conf.sourceRoots.toSet.contains(root2))
      }
    }

    it("should correctly merge with outer proj") {
      withTemporaryDirectory { dir =>
        val root0 = CanonFile(createUniqueDirectory(dir))
        val root1 = CanonFile(createUniqueDirectory(dir))
        val root2 = CanonFile(createUniqueDirectory(dir))
        val target = CanonFile(createUniqueDirectory(dir))
        val conf = parse(s"""
          |(
          |  :name "Outer"
          |  :target "$target"
          |  :source-roots ("$root0")
          |  :subprojects (
          |     (
          |     :name "Proj A"
          |     :module-name "A"
          |     :source-roots ("$root1")
          |     )
          |     (
          |     :name "Proj B"
          |     :module-name "B"
          |     :source-roots ("$root2")
          |     :depends-on-modules ("A")
          |     )
          |  )
          |  :active-subproject "B"
          |)""".stripMargin)
        assert(conf.name.get == "Proj B")
        assert(conf.sourceRoots.toSet.contains(root0))
        assert(conf.sourceRoots.toSet.contains(root1))
        assert(conf.sourceRoots.toSet.contains(root2))
        assert(conf.target.get == target)
      }
    }

    it("can parse nil as a string list") {
      withTemporaryDirectory { dir =>
        val conf = parse("(:source-roots nil )")
        assert(conf.sourceRoots.toSet == Set())
      }
    }

    it("should reject an incorrect list of strings") {
      withTemporaryDirectory { dir =>
        val root0 = CanonFile(createUniqueDirectory(dir))
        val conf = parse(s"""(:source-roots ("$root0" 1000))""")
        assert(conf.sourceRoots.toSet == Set())
      }
    }

    it("can parse a regex list") {
      val conf = parse("""( :only-include-in-index ("x") )""")
      assert(conf.onlyIncludeInIndex.map(_.toString()) == List("x"))
    }

    it("can parse nil as a regex list") {
      val conf = parse("( :only-include-in-index nil )")
      assert(conf.onlyIncludeInIndex.toList == List())
    }

    it("should reject an incorrect regex list") {
      val conf = parse("""( :only-include-in-index ("x" 2) )""")
      assert(conf.onlyIncludeInIndex.toList == List())
    }

  }

}
