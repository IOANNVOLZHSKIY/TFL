import com.sun.tools.javac.jvm.PoolConstant.LoadableConstant.String
import linearParser._

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

    val regex = "a(a|b)*"

    finderClosingBrac(regex, 0)

    print(parse(regex, true))

  }
}