import scala.util.Random

object regexGenerator {

  def starKellie(expr: String): String = {
    if (expr.size > 1) {
      return "(" + expr + ")" + "*"
    } else if (expr.size == 1) {
      return expr + "*"
    } else {
      return ""
    }
  }

  def getLetter(num: Int): String = {
    return ('a' + num).toChar.toString
  }

  def Alt (left: String, right: String): String = {
    if (left.nonEmpty && right.nonEmpty) {
      return "(" + left + "|" + right + ")"
    } else {
      return ""
    }
  }

  def recursionComp(letterNumber: Int, alphabetSize: Int): String = {
    if ((letterNumber <= 0) || (alphabetSize <= 0)) {
      return ""
    } else {
      val op = Random.nextInt(4)

      if (op == 0) {
        val l = recursionComp(letterNumber / 2, alphabetSize)
        val r = recursionComp(letterNumber / 2, alphabetSize)
        return l + r
      } else if (op == 1) {
        val l = recursionComp(letterNumber - 1, alphabetSize)
        val r = recursionComp(letterNumber - 1, alphabetSize)
        return Alt(l, r)
      } else if (op == 2) {
        val subExpr = recursionComp(letterNumber - 1, alphabetSize - 1)
        return starKellie(subExpr)
      } else {
        val letter = getLetter(Random.nextInt(alphabetSize))
        return letter
      }
    }
  }

  def regexGen(regexNumber: Int, alphSize: Int, starHeight: Int, letterNumber: Int): Array[String] = {
    var regexes = Array.ofDim[String](regexNumber)

    for (i <- 0 until regexNumber) {
      regexes(i) = recursionComp(letterNumber, alphSize)
    }

    return regexes
  }
}
