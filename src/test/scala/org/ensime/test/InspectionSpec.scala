package org.ensime.test
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import util.Helpers._

class InspectionSpec extends Spec with ShouldMatchers{
  
  describe("Symbol Info") {
    
    it("should see local variable declaration") {
      withPresCompiler{ cc =>
	val src = srcFile("Test.scala", contents(
	    "object Test{",
	    "def main{",
	    "val dude = 1",
	    "  ",
	    "}",
	    "}"
	  ))
	cc.askReloadAndRecompileFiles(List(src))
	val sym = cc.askSymbolInfoAt(src.position(3,5))
	sym.name should equal("dude")
      }
    }

  }

}
