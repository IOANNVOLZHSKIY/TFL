import com.sun.tools.javac.jvm.PoolConstant.LoadableConstant.String
import linearParser._
import glushkov._
import regexGenerator._

import scala.util.Random
import scala.io.StdIn.readInt
import scala.util.matching.Regex

object RegexUtils {
  implicit class RichRegex(val underlying: Regex) extends AnyVal {
    def matches(s: String) = underlying.pattern.matcher(s).matches
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    println("Enter parameters for regex generation")

    println("Regex Number:")
    val regexNumber = readInt()

    println("Alphabet Size:")
    val alphSize = readInt()

    println("Star height:")
    val starHeight = readInt()

    println("Number if letters:")
    val letterNumber = readInt()


    var test_array = regexGen(regexNumber, alphSize, starHeight, letterNumber)

    for (i <- 0 until test_array.length) {
      var reg = test_array(i)
      var automata = make_automata(reg)
      println("Automata:" + automata)

      var reachMx = matrixReachability(automata)
      println("Matrix:" + reachMx)

      var list_of_reach = listReachableFromItself(reachMx)
      println("List of hundred states that are achievable by themselves " + list_of_reach)

      var isCycle = false

      for (i <- 0 until list_of_reach.length) {
        if (last_qq.contains(list_of_reach(i))) {
          isCycle = true
        }
      }

      val random = new Random()

      for (k <- 0 until 10) {
        var res_word = ""

        if ((list_of_reach.length == 0) || (!isCycle)) {
          //print
          val rn = random.nextInt(last_qq.length)
          var last_q = last_qq(rn)
          //print
          res_word = createWord("S", last_q, automata)
        } else {
          //print
          var res = Vector[Any]("S")
          var next_step = list_of_reach
          var isContinue = 1

          while ((isContinue == 1) && (next_step.length != 0)) {
            var rn = random.nextInt(next_step.length - 1)
            res = res :+ next_step(rn)
            res = res :+ next_step(rn)

            if (!(last_qq.contains(next_step(rn)))) {
              isContinue = 1
            } else {
              isContinue = random.nextInt(1)
            }
          }

          //print

          res_word = ""

          for (i <- 0 until res.length - 1) {
            if (res(i) == res(i + 1)) {
              var repeat = random.between(600, 800)

              for (k <- 0 until repeat) {
                res_word += createCycle(res(i), res(i), automata, false)
              }
            } else {
              res_word += createCycle(res(i), res(i), automata, false)
            }
          }
        }

        res_word += "Ω"
        println(res_word)

        //Здесь необходимо вызвать нормализацию выражения в переменную pattern

        // Сравниваем pattern и res_word через функцию matches
        // Выводим разницу во времени работы

        //Проделываем тоже самое, но вместо pattern берем reg (сгенерированное выражение)
        //Показываем, что нормализация помогает и радуемся жизни

      }
    }
  }
}