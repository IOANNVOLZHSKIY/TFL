import model._
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
    var alphSize = readInt()
    while (!(1 to 25 contains(alphSize))) {
      println("The alphabet length must be greater than 0 but less than 25 characters long")
      alphSize = readInt()
    }

    println("Max star height:")
    val starHeight = readInt()

    println("Max number of letters in regex:")
    val letterNumber = readInt()

    val test_array = regexGen(regexNumber, alphSize, starHeight, letterNumber)

    for (i <- test_array.indices) {
      last_qq = Vector[Any]()
      val reg = test_array(i)
      println(reg)
      val automata = make_automata(reg)
      println("Automata:" + automata + "\n")

      val reachMx = matrixReachability(automata)
      println("Matrix:" + reachMx + "\n")

      var list_of_reach = listReachableFromItself(reachMx)

      var isCycle = false

      for (i <- list_of_reach.indices) {
        if (last_qq.contains(list_of_reach(i))) {
          isCycle = true
        }
      }

      var upd_list_of_reach = Vector[Any]()

      for (i <- last_qq.indices) {
        for (t <- list_of_reach.indices) {
          if (reachMx(indexFinder(list_of_reach(t).toString, reachMx))._2.contains(last_qq(i))) {
            if (reachMx(indexFinder(last_qq(i).toString, reachMx))._2.contains(list_of_reach(t))) {
              upd_list_of_reach = upd_list_of_reach :+ list_of_reach(t)
            }
          }
        }
      }

      list_of_reach = upd_list_of_reach

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
              val repeat = random.between(50, 200)

              for (k <- 0 until repeat) {
                res_word += createCycle(res(i).toString, res(i).toString, automata, reachMx, false)
              }
            } else {
              res_word += createWord(res(i).toString, res(i + 1).toString, automata, reachMx)
            }
          }
        }

        val op = Random.nextInt(2)

        if (op == 1) {
          // Злонамеренная накачка
          val alphabet = "abcdefghijklmnopqrstuvwxyz"
          val missingChar = alphabet.find(char => !res_word.contains(char))
          val resultString = missingChar.map(char => res_word + char).getOrElse(res_word)
          res_word = resultString
          println("Malicious pumping")
        }

        val pattern = normalizeRegex(reg)

        println("Result word:" + res_word)

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