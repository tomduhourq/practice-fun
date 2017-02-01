package aperiodic

import scala.annotation.tailrec

class P05[A] {
  def reverse(list: List[A]): List[A] = {
    @tailrec
    def reverseRec(partial: List[A], left: List[A]): List[A] = left match {
      case Nil => partial
      case head :: tail => reverseRec(head :: partial, tail)
    }
    reverseRec(Nil, list)
  }
}

object P05 extends App {
  val p05 = new P05[Int]
  assert(p05.reverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1))
  assert(p05.reverse(List()) == List())
  assert(p05.reverse(List(1)) == List(1))
}
