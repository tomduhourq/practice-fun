package aperiodic

import scala.annotation.tailrec

class P09[A] {
  def pack(list: List[A]): List[List[A]] = {
    @tailrec
    def packRec(partial: List[List[A]], current: A, howMany: Int, left: List[A]): List[List[A]] = left match {
      case Nil => partial :+ List.fill(howMany)(current)
      case head :: tail if current == head => packRec(partial, current, howMany + 1, tail)
      case head :: tail => packRec(partial :+ List.fill(howMany)(current), head, 1, tail)
    }
    if (list.isEmpty) Nil
    else {
      val head = list.head
      packRec(Nil, head, 1, list.tail)
    }
  }
}

object P09 extends App {
  val p09 = new P09[Int]
  assert(p09.pack(List()) == List())
  assert(p09.pack(List(1, 1)) == List(List(1, 1)))
  assert(p09.pack(List(1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5)) == List(List(1, 1, 1, 1), List(2), List(3, 3), List(1, 1), List(4), List(5, 5, 5, 5)))
}
