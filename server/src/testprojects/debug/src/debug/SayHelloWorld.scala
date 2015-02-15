package debug

import scala.io.StdIn

object SayHelloWorld {

  def main(args: Array[String]) {
    val name = StdIn.readLine()
    println(s"Hello, $name")
  }

}