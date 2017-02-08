package aperiodic

import scala.util.Try

class P17[A] {
  def split(at: Int, list: List[A]): (List[A], List[A]) = {
    require(at <= list.length, s"At parameter: $at cannot be larger than list size: ${list.length}")
    (list.take(at), list.drop(at))
  }
}

object P17 extends App {
  val p17 = new P17[Int]
  Try { p17.split(2, List()) } recover { case _: IllegalArgumentException => println("Success!") }
  assert(p17.split(2, List(1, 2, 3, 4, 5, 6)) == (List(1, 2), List(3, 4, 5, 6)))
}
