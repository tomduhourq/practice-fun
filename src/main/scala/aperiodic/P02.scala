package aperiodic

import scala.annotation.tailrec

class P02[A] {
  def penultimate(list: List[A]): Option[A] = {
    @tailrec
    def penultimateRec(left: List[A]): Option[A] = left match {
      case Nil => None
      case penultimate :: _ :: Nil => Some(penultimate)
      case _ :: tail => penultimateRec(tail)
    }
    penultimateRec(list)
  }
}

object P02 extends App {
  val p02 = new P02[Int]
  assert(p02.penultimate(List(1,2,3)) contains 2)
  assert(p02.penultimate(List()).isEmpty)
  assert(p02.penultimate(List(1)).isEmpty)
  assert(p02.penultimate(List(1,2)) contains 1)
}
