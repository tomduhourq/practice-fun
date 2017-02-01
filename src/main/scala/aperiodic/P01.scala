package aperiodic

import scala.annotation.tailrec

class P01[A] {
  def last(list: List[A]): Option[A] = {
    @tailrec
    def lastRec(left: List[A]): Option[A] = left match {
      case Nil => None
      case last :: Nil => Some(last)
      case _ :: tail => lastRec(tail)
    }
    lastRec(list)
  }
}

object P01 extends App {
  val p01 = new P01[Int]
  assert(p01.last(List(1,2,3)) contains 3)
  assert(p01.last(List()).isEmpty)
  assert(p01.last(List(1)) contains 1)
}
