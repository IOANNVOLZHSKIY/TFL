import com.sun.tools.javac.jvm.PoolConstant.LoadableConstant.String
import scala.io.StdIn.{readInt, readLine}
import scala.util.Random

import regexGenerator._

object Main {

  def main(args: Array[String]): Unit = {
    /*println("Enter parameters for regex generation")

    println("Regex Number:")
    val regexNumber = readInt()

    println("Alphabet Size:")
    val alphSize = readInt()

    println("Star height:")
    val starHeight = readInt()

    println("Number if letters:")
    val letterNumber = readInt()

     */

    println(regexGen(2, 5, 3, 15)(0))
    println(regexGen(2, 5, 3, 15)(1))
  }
}