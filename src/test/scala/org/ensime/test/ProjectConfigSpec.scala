package org.ensime.test
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.ensime.config.ProjectConfig
import org.ensime.util.SExp
import org.ensime.util.FileUtils._
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

    it("should parse a simple name correctly") {
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
	  "     :module-name \"A\"",
	  "     )",
	  "     (",
	  "     :name \"Goodbye\"",
	  "     :module-name \"B\"",
	  "     )",
	  "  )",
	  "  :active-subproject \"B\"",
	  ")"
	))
      assert(conf.name.get == "Goodbye")
    }

    it("should correctly merge dependency") {
      withTemporaryDirectory{ dir =>
	val root1 = CanonFile(createUniqueDirectory(dir))
	val root2 = CanonFile(createUniqueDirectory(dir))
	val conf = parse(List(
	    "(",
	    "  :subprojects (",
	    "     (",
	    "     :name \"Proj A\"",
	    "     :module-name \"A\"",
	    "     :source-roots (\"" + root1 + "\")",
	    "     )",
	    "     (",
	    "     :name \"Proj B\"",
	    "     :module-name \"B\"",
	    "     :source-roots (\"" + root2 + "\")",
	    "     :depends-on-modules (\"A\")",
	    "     )",
	    "  )",
	    "  :active-subproject \"B\"",
	    ")"
	  ))
	assert(conf.name.get == "Proj B")
	println(conf.sourceRoots)
	assert(conf.sourceRoots.toSet.contains(root1))
	assert(conf.sourceRoots.toSet.contains(root2))
      }
    }


    it("should correctly merge with outer proj") {
      withTemporaryDirectory{ dir =>
	val root0 = CanonFile(createUniqueDirectory(dir))
	val root1 = CanonFile(createUniqueDirectory(dir))
	val root2 = CanonFile(createUniqueDirectory(dir))
	val target = CanonFile(createUniqueDirectory(dir))
	val conf = parse(List(
	    "(",
	    "     :name \"Outer\"",
	    "     :target \"" + target + "\"",
	    "     :source-roots (\"" + root0 + "\")",
	    "  :subprojects (",
	    "     (",
	    "     :name \"Proj A\"",
	    "     :module-name \"A\"",
	    "     :source-roots (\"" + root1 + "\")",
	    "     )",
	    "     (",
	    "     :name \"Proj B\"",
	    "     :module-name \"B\"",
	    "     :source-roots (\"" + root2 + "\")",
	    "     :depends-on-modules (\"A\")",
	    "     )",
	    "  )",
	    "  :active-subproject \"B\"",
	    ")"
	  ))
	assert(conf.name.get == "Proj B")
	println(conf.sourceRoots)
	assert(conf.sourceRoots.toSet.contains(root0))
	assert(conf.sourceRoots.toSet.contains(root1))
	assert(conf.sourceRoots.toSet.contains(root2))
	assert(conf.target.get == target)
      }
    }


  }

}
