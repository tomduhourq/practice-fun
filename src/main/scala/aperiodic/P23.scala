package aperiodic

import scala.util.{Random, Try}

class P23[A] extends P20[A] {
  def randomSelect(n: Int, list: List[A]): List[A] = {
    require(n <= list.length, s"Cannot select $n randomly elements from $list")

    val (_, randoms) = (1 to n).foldLeft((list, List.empty[A])) {
      case ((current, partial), _) =>
        val random = Random.nextInt(current.length)
        val (newCurrent, Some(element)) = removeAt(random, current)
        (newCurrent, element :: partial)
    }

    randoms
  }
}

object P23 extends App {
  val p23 = new P23[Symbol]
  println(p23.randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
  Try { p23.randomSelect(3, List()) } recover { case _: IllegalArgumentException => println("Success!") }
}
