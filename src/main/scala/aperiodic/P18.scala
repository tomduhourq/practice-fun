package aperiodic

import scala.util.Try

class P18[A] {
  def slice(from: Int, until: Int, list: List[A]): List[A] = {
    require(from >= 0 && from <= list.length - 1, s"From parameter: $from cannot be greater than list size and should be greater or equal to 0.")
    require(until >= 0 && from <= until && until <= list.length - 1, s"Until parameter: $until should be greater or equal to 0, greater than from: $from and cannot be greater than list size")
    list.drop(from).take(until - from)
    // list.slice(from, until)
  }
}

object P18 extends App {
  val p18 = new P18[Symbol]
  Try { p18.slice(-1, 2, List()) } recover { case _: IllegalArgumentException => println("Success!") }
  Try { p18.slice(2, 2, List('a)) } recover { case _: IllegalArgumentException => println("Success!") }
  Try { p18.slice(2, -1, List('a, 'b)) } recover { case _: IllegalArgumentException => println("Success!") }
  Try { p18.slice(2, 1, List('a, 'b)) } recover { case _: IllegalArgumentException => println("Success!") }
  Try { p18.slice(1, 0, List('a)) } recover { case _: IllegalArgumentException => println("Success!") }
  assert(p18.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g))
}
