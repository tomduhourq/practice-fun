package aperiodic

import scala.util.Try

class P15[A] {
  def duplicateN(times: Int, list: List[A]): List[A] = {
    require(times >= 1, "times should be greater or equal to 1.")
    list flatMap (element => List.fill(times)(element))
  }
}

object P15 extends App {
  val p15 = new P15[Symbol]
  assert(p15.duplicateN(1, List()) == List())
  Try(p15.duplicateN(-1, List())) recover { case _: IllegalArgumentException => println("Success!") }
  assert(p15.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
}
