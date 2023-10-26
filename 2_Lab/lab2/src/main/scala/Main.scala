import com.sun.tools.javac.jvm.PoolConstant.LoadableConstant.String
import model._
import linearParser._
import glushkov._
import regexGenerator._

import scala.util.{Random, Try}
import scala.io.StdIn.readInt
import scala.util.matching.Regex

object RegexUtils {
  implicit class RichRegex(val underlying: Regex) extends AnyVal {
    def matches(s: String) = underlying.pattern.matcher(s).matches
  }
}

object Main {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def normalizeRegex(regex: String): String = {
    if (regex.isBlank || regex.isEmpty) "empty"
    else {
      val parsed: Option[Term] = Try(RegexParser(regex)).toOption.flatten
      parsed match {
        case Some(result) =>
          val tree = RegexTree(Term.applySSNF(result))
          Term.transformToLeftAssociativity(tree.root)
          Term.normalizeAlternatives(tree.root, isLeftChild = false, parent = tree)
          Term.applyDstr(tree.root, isLeftChild = false, parent = tree)
          tree.toPrettyRegex
        case None => "not parsed"
      }
    }
  }

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

    val test_array = regexGen(regexNumber, alphSize, starHeight, letterNumber)


    for (i <- 0 until test_array.length) {
      last_qq = Vector[Any]()
      var reg = test_array(i)
      println(reg)
      var automata = make_automata(reg)
      println("Automata:" + automata)

      var reachMx = matrixReachability(automata)
      println("Matrix:" + reachMx)

      var list_of_reach = listReachableFromItself(reachMx)
      println("List of hundred states that are achievable by themselves: " + list_of_reach)

      var isCycle = false

      for (i <- 0 until list_of_reach.length) {
        if (last_qq.contains(list_of_reach(i))) {
          isCycle = true
        }
      }

      val random = new Random()

      for (k <- 0 until 10) {
        var res_word = ""

        if (list_of_reach.isEmpty || (!isCycle)) {
          val rn = random.nextInt(last_qq.length)
          val last_q = last_qq(rn)
          last_q match {
            case l: String =>
              res_word = createWord("S", l, automata, reachMx)
          }
        } else {
          var res = Vector[Any]("S")
          val next_step = list_of_reach
          var isContinue = 1

          while ((isContinue == 1) && next_step.nonEmpty) {
            var rn = random.nextInt(next_step.length)
            res = res :+ next_step(rn)
            res = res :+ next_step(rn)

            if (!(last_qq.contains(next_step(rn)))) {
              isContinue = 1
            } else {
              isContinue = random.nextInt(2)
            }
          }

          res_word = ""

          for (i <- 0 until res.length - 1) {
            if (res(i) == res(i + 1)) {
              val repeat = random.between(600, 800)

              for (k <- 0 until repeat) {
                res_word += createCycle(res(i).toString, res(i).toString, automata, reachMx, false)
              }
            } else {
              res_word += createWord(res(i).toString, res(i + 1).toString, automata, reachMx)
            }
          }
        }

        val pattern = normalizeRegex(reg)

        println("Time with normalized regex: ")

        val t = time {
          if (res_word matches pattern) {
            println("Match")
          }
          else {
            println("Doesn't match")
          }
        }

        println("Time with original regex: ")

        val t1 = time {
          if (res_word matches reg) {
            println("Match")
          } else {
            println("Doesn't match")
          }
        }
        println()
      }
    }
  }
}