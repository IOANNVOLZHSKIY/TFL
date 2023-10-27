import jdk.dynalink.linker.support.Guards.isInstance
import linearParser._

import scala.util.Random

object glushkov {
  def first(regex: Any): (Vector[Any], Boolean) = { // предусмотреть второй случай, где нужно вывести без булеан
    regex match {
      case regex1: String =>
        return (Vector(regex1), false)
      case regex1: Char =>
        return (Vector(regex1), false)
      case regex1: Vector[Any] =>
        if (regex1.length == 1) {
          return first(regex1(0))
        } else if ((regex1.length == 2) && (regex1(1) == "*")) {
          return (first(regex1(0))._1, true)
        } else if ((regex1.length >= 3) && (regex1(1) == "&")) {
          val res = first(regex1(0))
          val start = res._1
          val isKellie = res._2

          if (isKellie) {
            val res1 = first(regex1.slice(2, regex1.length))

            return ((Vector.concat(res1._1, start), res1._2))
          } else {
            return (start, false)
          }
        } else {
          var res = Vector[Any]()
          var isKellie = false

          for (i <- regex1.indices by 2) {
            val temp = first(regex1(i))

            res = Vector.concat(res, temp._1)
            val isKellie = temp._2
          }

          return (res, isKellie)
        }
    }
  }

  def last(regex: Any): (Vector[Any], Boolean) = { // предусмотреть второй случай, где нужно вывести без булеан
    regex match {
      case r: String =>
        return (Vector(regex), false)
      case r: Vector[Any] =>
        if (r.length == 1) {
          return first(r(0))
        } else if ((r.length == 2) && (r(1) == "*")) {
          return (first(r(0))._1, true)
        } else if ((r.length >= 3) && (r(1) == "&")) {
          val res = first(r.last)
          val start = res._1
          val isKellie = res._2

          if (isKellie) {
            val res1 = first(r.slice(0, r.length - 2))

            return (Vector.concat(res1._1, start), res1._2)
          } else {
            return (start, false)
          }
        } else {
          //println("reg", r)
          var res = Vector[Any]()
          var isKellie = false

          for (i <- r.indices by 2) {
            val temp = last(r(i))

            res = Vector.concat(res, temp._1)
            val isKellie = temp._2
          }

          return (res, isKellie)
        }
    }
  }

  def follow_s(regex: String, variable: String): Vector[Any] = {
    return Vector()
  }

  def follow(regex: Vector[Any], variable: String): Vector[Any] = {
    if (regex.length == 1) {
      regex.head match {
        case regex1: Vector[Any] =>
          return follow(regex1, variable)
        case _ => return Vector()
      }
    } else if ((regex.length == 2) && (regex(1) == "*")) {
      var res = Vector[Any]()
      regex.head match {
        case regex1: Vector[Any] =>
          var res = follow(regex1, variable)
          if (last(regex1)._1.contains(variable)) {
            res = Vector.concat(res, first(regex1)._1)
          }
          return res
        case regex1: String =>
          var res = follow_s(regex1, variable)
          return res
      }
    } else if ((regex.length >= 3) && (regex(1) == "|")) {
      var res = Vector[Any]()

      for (i <- regex.indices by 2) {
        regex(i) match {
          case regex1: Vector[Any] =>
            res = Vector.concat(res, follow(regex1, variable))
          case regex1: String =>
            var res = follow_s(regex1, variable)
        }
      }

      return res
    } else if ((regex.length >= 3) && (regex(1) == "&")) {
      var res = Vector[Any]()
      var status = true

      for (i <- regex.indices by 2) {

        regex(i) match {
          case regex1: Vector[Any] =>
            res = Vector.concat(res, follow(regex1, variable))

            if (last(regex1)._1.contains(variable)) {
              for (j <- i + 2 until regex.length by 2) {
                if (status) {
                  res = Vector.concat(res, first(regex(j))._1)

                  if (!(regex1.length == 2 && regex1(1) == "*")) {
                    status = false
                  }
                }
              }
            }
          case regex1: String =>
            res = Vector.concat(res, follow_s(regex1, variable))

            if (last(regex1)._1.contains(variable)) {
              for (j <- i + 2 until regex.length by 2) {
                if (status) {
                  res = Vector.concat(res, first(regex(j))._1)

                  if (!(regex1.length == 2 && regex1(1) == '*')) {
                    status = false
                  }
                }
              }
            }
        }
      }
      return res
    } else {
      return Vector("!")
    }
  }

  var last_qq = Vector[Any]()

  def make_automata(regex: String): Vector[(String, String, String)] = { //Запись автомата: [('S', 'a', 'a1'), ('a1', 'b', 'b2'), ('b2', 'a', 'a1')]
    val r = parse(regex, false)
    val f = first(r)._1

    last_qq = last(r)._1

    var res = Vector[(String, String, String)]()

    for (i <- f.indices) {

      f(i) match {
        case f1: Vector[Any] =>
          val tupleToAdd: (String, String, String) = ("S", f1(0).toString.slice(0, 1), f1(0).toString)
          res = res :+ tupleToAdd
        case f1: String =>
          val tupleToAdd: (String, String, String) = ("S", f1.slice(0, 1), f1)
          res = res :+ tupleToAdd
      }
    }

    for (i <- variables.indices) {
      r match {
        case r1: Vector[Any] =>
          val follows = follow(r1, variables(i))

          for (j <- follows.indices) {
            val temp = follows(j).toString
            val tupleToAdd: (String, String, String) = (variables(i), temp(0).toString, follows(j).toString)
            res = res :+ tupleToAdd
          }
        case r1: String =>
          var res = follow_s(r1, variables(i))
      }
    }

    return res
  }

  var used_q = Vector[Any]()

  def varReachability(q: String, automata: Vector[(String, String, String)]): Vector[Any] =  {
    var res = Vector[Any]()

    for (i <- automata.indices) {
      if (automata(i)._1 == q) {
        if (!(used_q.contains(automata(i)._3))) {
          res = res :+ automata(i)._3
          used_q = used_q :+ automata(i)._3
          res = Vector.concat(res, varReachability(automata(i)._3(0).toString, automata))
        }
      }
    }

    return res
  }

  def matrixReachability(automata: Vector[(String, String, String)]): Vector[(String, Vector[Any])] = { //Представление: [('S', ['a1', 'b2']), ('a1', ['b2', 'a1']), ('b2', ['a1', 'b2'])]
    var res = Vector[(String, Vector[Any])]()
    val tupleToAdd: (String, Vector[Any]) = ("S", variables.toVector)
    res = res :+ tupleToAdd

    for (i <- variables.indices) {
      used_q = Vector[Any]()
      val tupleToAdd: (String, Vector[Any]) = (variables(i), (varReachability(variables(i), automata)))
      res = res :+ tupleToAdd
    }

    return res
  }

  def listReachableFromItself(m: Vector[(String, Vector[Any])]): Vector[Any] = {
    var res = Vector[Any]()

    for (i <- m.indices) {
      if (m(i)._2.contains(m(i)._1)) {
        res = res :+ m(i)._1
      }
    }

    return res
  }

  def finderNextStep(q: String, ReachableFromInself: Vector[Any], ReachMatrix: Vector[(String, Vector[Any])]): Vector[Any] = {
    for (i <- ReachMatrix.indices) {
      if (ReachMatrix(i)._1 == q) {
        var next_step = Vector[Any]()

        for (j <- ReachMatrix(i)._2.indices) {
          if (ReachableFromInself.contains(ReachMatrix(i)._2(j))) {
            next_step = next_step :+ ReachMatrix(i)._2(j)
          }
        }
        return next_step
      }
    }

    return Vector()
  }

  def indexFinder(element: String, m: Vector[(String, Vector[Any])]): Int = {
    var res = Vector[Int]()

    for (i <- m.indices) {
      if (m(i)._1 == element) {
        res = res :+ i
      }
    }

    if (res.length == 1) {
      return res(0)
    } else {
      println("ERROR in indexFinder")
      return -1
    }
  }

  var done = false
  val random = new Random()
  var nq = ""

  def createCycle(Q: String, last: String, automat: Vector[(String, String, String)], reachMx: Vector[(String, Vector[Any])], done: Boolean): String = {
    var next_q = Vector[String]()
    var word = ""
    var done_new = done

    if (!done) {
      for (i <- automat.indices) {
        if ((automat(i)._1 == Q) && (reachMx(indexFinder(automat(i)._3, reachMx))._2.contains(last))) {
          next_q = next_q :+ automat(i)._3
        }
      }

      if (next_q.length == 1) {
        if (next_q(0) == Q) {
          word += next_q(0)(0)
        } else {
          word += next_q(0)(0)
        }

        var nq = next_q(0)
      } else {
        val r = random.nextInt(next_q.length)
        nq = next_q(r)

        if (nq == Q) {
          var wn = nq(0)
          word += wn
        } else {
          word += nq(0)
        }
      }

      if (nq == last) {
        done_new = true
      }
      return word + createCycle(nq, last, automat, reachMx, done_new)
    } else {
      return ""
    }
  }

  def createWord(first_q: String, last_q: String, automat: Vector[(String, String, String)], reachMx: Vector[(String, Vector[Any])]): String = {
    var word = ""
    var start_pos = first_q
    var next_q = Vector[String]()

    if ((first_q == last_q) && (reachMx(indexFinder(first_q, reachMx))._2.contains(last_q))) {
      return ""
    }

    if (reachMx(indexFinder(first_q, reachMx))._2.contains(last_q)) {
      for (i <- automat.indices) {
        if (automat(i)._1 == first_q) {
          if ((reachMx(indexFinder(automat(i)._3, reachMx))._2.contains(last_q)) || (last_q == automat(i)._3)) {
            next_q = next_q :+ automat(i)._3
          }
        }
      }

      if (next_q.isEmpty) {
        return last_q(0).toString
      }

      if (next_q.length == 1) {
        if (next_q(0) == first_q) {
          val rn = random.between(100, 300)

          for (k <- 0 until rn) {
            word += next_q(0)(0)
          }
        } else {
          word += next_q(0)(0)
        }
        nq = next_q(0)
      } else {

        val rn = random.nextInt(next_q.length)
        nq = next_q(rn)

        if (nq == first_q) {
          val rn = random.between(100, 300)
          var wn = ""
          for (k <- 0 until rn) {
            wn += nq(0)
          }
        } else {
          word += nq(0)
        }
      }

      return word + createWord(nq, last_q, automat, reachMx)
    }
    else {
      return ""
    }
  }
}
