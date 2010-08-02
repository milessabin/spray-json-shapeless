package org.ensime.test
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import util.Helpers._

class InspectionSpec extends Spec with ShouldMatchers{
  
  describe("Symbol Info") {

    it("should see local variable declaration") {
      withPresCompiler{ cc =>
	val src = srcFile("Test1.scala", contents(
	    "object Test1{",
	    "def main{",
	    "val dude = 1",
	    "  ",
	    "}",
	    "}"
	  ))
	cc.askReloadAndTypeFiles(List(src))
	val info = cc.askSymbolInfoAt(src.position(2,5))
	info.name should equal("dude")
      }
    }
    
    it("should find local declaration position ") {
      withPresCompiler{ cc =>
    	val src = srcFile("Test2.scala", contents(
    	    "object Test2{",
    	    "def main{",
    	    "val dude = 1",
    	    "//space",
    	    "//space",
    	    "val horse = dude",
    	    "}",
    	    "}"
    	  ))
    	cc.askReloadAndTypeFiles(List(src))
    	val info = cc.askSymbolInfoAt(src.position(5,12))
    	info.declPos.line should equal(3)
      }
    }

  }


  describe("Type Info") {

    it("should get type of local variable variable declaration") {
      withPresCompiler{ cc =>
	val src = srcFile("Test1.scala", contents(
	    "object Test1{",
	    "def main{",
	    "val dude = 1",
	    "  ",
	    "}",
	    "}"
	  ))
	cc.askReloadAndTypeFiles(List(src))
	val info = cc.askTypeInfoAt(src.position(2,5))
	info.name should equal("Int")
      }
    }
    
    it("should get type of usage of local var ") {
      withPresCompiler{ cc =>
    	val src = srcFile("Test2.scala", contents(
    	    "object Test2{",
    	    "def main{",
    	    "val dude = 1",
    	    "//space",
    	    "//space",
    	    "val horse = dude",
    	    "}",
    	    "}"
    	  ))
    	cc.askReloadAndTypeFiles(List(src))
    	val info = cc.askTypeInfoAt(src.position(5,12))
	info.name should equal("Int")
      }
    }

  }




}
