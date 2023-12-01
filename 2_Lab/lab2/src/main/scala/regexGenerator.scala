import scala.util.Random

object regexGenerator {

  def starKellie(expr: String): String = {
    if (expr.length > 1) {
      return "(" + expr + ")" + "*"
    } else if (expr.length == 1) {
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

  def recursionComp(maxLength: Int, maxStarHeight: Int, alphabetSize: Int): String = {
    if ((maxLength <= 0) || (maxStarHeight <= 0)) {
      return ""
    } else if (maxLength == 1) {
      val letter = getLetter(Random.nextInt(alphabetSize))
      return letter
    } else {
      val op = Random.nextInt(4)
      val lenL = Random.nextInt(maxLength)
      val lenR = maxLength - lenL

      if (op == 0) {
        val l = recursionComp(lenL, maxStarHeight, alphabetSize)
        val r = recursionComp(lenR, maxStarHeight, alphabetSize)
        return l + r
      } else if (op == 1) {
        val l = recursionComp(lenL, maxStarHeight, alphabetSize)
        val r = recursionComp(lenR, maxStarHeight, alphabetSize)
        return Alt(l, r)
      } else if (op == 2) {
        val letter = getLetter(Random.nextInt(alphabetSize))
        return letter
      } else {
        val subExpr = recursionComp(maxLength - 1, maxStarHeight - 1, alphabetSize)
        return starKellie(subExpr)
      }
    }
  }

  def regexGen(regexNumber: Int, alphSize: Int, starHeight: Int, letterNumber: Int): Array[String] = {
    var regexes = Array.ofDim[String](regexNumber)

    for (i <- 0 until regexNumber) {
      regexes(i) = recursionComp(letterNumber, starHeight, alphSize)
    }

    return regexes
  }
}
