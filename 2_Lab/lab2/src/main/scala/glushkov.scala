import jdk.dynalink.linker.support.Guards.isInstance
import linearParser._

object glushkov {
  def first(regex: String): (Vector[Any], Boolean) = { // предусмотреть второй случай, где нужно вывести без булеан
    if (regex.isInstanceOf[String]) {
      return (Vector(regex), false)
    } else if (regex.length == 1) {
      return first(regex(0).toString)
    } else if ((regex.length == 2) && (regex(1) == '*')) {
      return (first(regex(0).toString)._1, true)
    } else if ((regex.length >= 3) && (regex(1) == '&')) {
      val res = first(regex(0).toString)
      val start = res._1
      val isKellie = res._2

      if (isKellie) {
        val res1 = first(regex.slice(2, regex.length))

        return (res1._1, res1._2)
      } else {
        return (start, false)
      }
    } else {
      var res = Vector[Any]()
      var isKellie = false

      for (i <- 0 until regex.length by 2) {
        val temp = first(regex(i).toString)

        res = res :+ temp._1
        val isKellie = temp._2
      }

      return (res, isKellie)
    }
  }

}
