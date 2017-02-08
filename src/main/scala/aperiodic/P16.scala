package aperiodic

import scala.annotation.tailrec
import scala.util.Try

class P16[A] {
  def drop(howMany: Int, list: List[A]): List[A] = {
    require(howMany >= 1, "Number must be greater than 1.")
    @tailrec
    def dropRec(partial: List[A], count: Int, left: List[A]): List[A] = left match {
      case Nil => partial
      case head :: tail if count > 1 => dropRec(partial :+ head, count - 1, tail)
      case _ :: tail => dropRec(partial, howMany, tail)
    }
    dropRec(Nil, howMany, list)
  }

  def dropFunctional(howMany: Int, list: List[A]): List[A] = list grouped howMany flatMap {
    case group if group.length == howMany => group.init
    case group => group
  } toList
}

object P16 extends App {
  val p16 = new P16[Symbol]
  Try(p16.drop(-1, List())) recover { case _: IllegalArgumentException => println("Success!") }
  assert(p16.drop(3, List()) == List())
  assert(p16.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  assert(p16.dropFunctional(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
}
