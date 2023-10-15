import scala.concurrent.ExecutionContext.global

object linearParser {
  var index = 0
  var variables = List[String]()

  def finderClosingBrac(regex: String, open_id: Int): Int = {
    var counter = 0
    var res = true

    var i = open_id + 1

    while (i < regex.length) {
      if (regex(i) == '(') {
        counter += 1
      } else if (regex(i) == ')') {
        if (counter == 0) {
          res = false
          return i
        } else {
          counter -= 1
        }
      }

      i += 1
    }

    if (!res) {
      return i - 1
    } else {
      //println("ERROR")
      return -1
    }
  }

  def notOutOr(regex: String): Boolean = {
    var i = 0

    while (i < regex.length) {
      if (regex(i) == '|') {
        return false
      } else if (regex(i) == '(') {
        if (i == -1) {
          println("ERROR NOTOUTOR")
        } else {
          i = finderClosingBrac(regex, i)
        }
      }
      i += 1
    }

    return true
  }

  def parse(regex: String, status: Boolean): Any = {
    if (!status) {
      if (regex.isEmpty) {
        return regex
      } else {
        var res = ""

        for (i <- 0 until regex.length - 1) {
          if ((regex(i + 1) == '*') && (regex(i) != ')')) { //Star Kellie brackets
            res += "(" + regex(i) + ")"
          } else {
            res += regex(i)
          }
        }

        res += regex.last
        parse(res, true)
      }
    } else {
      if (regex.length == 1) { // Возможно литера
        if ((regex == "|") || (regex == "&")) {
          return regex
        } else {
          index += 1
          var new_lit = regex + index.toString // Линеаризация

          variables = variables :+ new_lit

          return new_lit
        }
      } else if ((regex(0) == '(') && (finderClosingBrac(regex, 0) == regex.length - 2) && (regex.last == '*')) {
        if (regex.length == 4) {
          return Vector(Vector(regex(1)), "*")
        } else {
          return Vector(parse(regex.slice(1, regex.length - 2), true), "*")
        }
      } else if ((regex(0) == '(') && (finderClosingBrac(regex, 0) == regex.length - 1)) {
        if (regex.length == 3) {
          return List(regex(1))
        } else {
          //println(regex.slice(1, regex.length - 1))
          return parse(regex.slice(1, regex.length - 1), true)
        }
      } else if (notOutOr(regex)) {
        var l = Vector[Any]()
        var start_index = 0
        var i = 0

        while (i < regex.length) {
          if (regex(i) != '(') {
            if ((i != regex.length - 1) && (regex(i + 1) == '*')) {
              val elementToAdd = List[String](regex.slice(start_index, i + 1), "*")
              l = l :+ elementToAdd
              i += 1
              start_index = i + 2
            } else {
              l = l :+ regex.slice(start_index, i + 1)
              start_index = i + 1
            }

            l = l :+ "&"
          } else {
            i = finderClosingBrac(regex, i)

            if ((i < regex.length - 1) && (regex(i + 1) == '*')) {
              i += 1
            }

            l :+= regex.slice(start_index, i + 1)
            l :+= "&"

            start_index = i + 1
          }

          i += 1
        }

        for (k <- l.indices) {
          l = l.updated(k, parse(l(k).toString, true))
        }

        return l.slice(0, l.size - 1)
      }

      var l = Vector[Any]()
      var start_index = 0
      var i = 0

      while (i < regex.length){
        if (regex(i) == '(') {
          i = finderClosingBrac(regex, i)
        } else if (regex(i) == '|'){

          l = l :+ regex.slice(start_index, i)
          l = l :+ "|"
          start_index = i + 1
        }

        i += 1
      }

      l = l :+ regex.slice(start_index, i)

      for (k <- l.indices) {
        l = l.updated(k, parse(l(k).toString, true))
      }

      return l
    }
  }
}
