package org.ensime.test
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.ensime.config.ProjectConfig
import org.ensime.util.SExp
import org.ensime.util.CanonFile


class ProjectConfigSpec extends Spec with ShouldMatchers{

  def parse(s:String): ProjectConfig = {
    ProjectConfig.fromSExp(SExp.read(s)) match{
      case Right(c) => c
      case Left(t) => throw t
    }
  }

  def parse(ss:List[String]): ProjectConfig = {
    parse(ss.mkString("\n"))
  }

  describe("ProjectConfigSpec") {

    it("should parse a simple name right") {
      assert(parse("(:name \"dude\")").name.get == "dude")
    }

    it("should parse name's synonym") {
      assert(parse("(:project-name \"dude\")").name.get == "dude")
    }

    it("should respect active-subproject") {
      val conf = parse(List(
	"(",
	"  :subprojects (",
	"     (",
	"     :name \"Hello\"",
	"     )",
	"     (",
	"     :name \"Goodbye\"",
	"     )",
	"  )",
	"  :active-subproject \"Hello\"",
	")"
      ))
      assert(conf.name.get == "Hello")
    }

    it("should correctly merge dependency") {
      val conf = parse(List(
	"(",
	"  :subprojects (",
	"     (",
	"     :name \"A\"",
	"     :source-roots (\"a-src\")",
	"     )",
	"     (",
	"     :name \"B\"",
	"     :source-roots (\"b-src\")",
	"     :dependent-subprojects (\"A\")",
	"     )",
	"  )",
	"  :active-subproject \"B\"",
	")"
      ))
      assert(conf.name.get == "B")
      println(conf.sourceFilenames)
      assert(conf.sourceRoots.toSet.contains(CanonFile("a-src")))
      assert(conf.sourceRoots.toSet.contains(CanonFile("b-src")))
    }

  }

}
