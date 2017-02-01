package aperiodic

import scala.annotation.tailrec

class P03[A] {
  def nth(n: Int, list: List[A]): Option[A] = {
    require(n >= 0)
    @tailrec
    def nthRec(k: Int, left: List[A]): Option[A] = left match {
      case Nil       if k >= 0  => None
      case elem :: _ if k == 0 => Some(elem)
      case _ :: tail           => nthRec(k - 1, tail)
    }
    nthRec(n, list)
  }
}

object P03 extends App {
  val p03 = new P03[Int]
  assert(p03.nth(2, List(1,2,3)) contains 3)
  assert(p03.nth(4, List()).isEmpty)
  assert(p03.nth(0, List(1)) contains 1)
}
