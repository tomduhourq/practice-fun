package aperiodic

import scala.annotation.tailrec

class P08[A] {
  def compress(list: List[A]): List[A] = {
    @tailrec
    def compressRec(partial: List[A], current: A, left: List[A]): List[A] = left match {
      case Nil => partial
      case head :: tail if current == head => compressRec(partial, current, tail)
      case head :: tail => compressRec(partial :+ head, head, tail)
    }
    if (list.isEmpty) Nil
    else {
      val head = list.head
      compressRec(List(head), head, list.tail)
    }
  }
}

object P08 extends App {
  val p08 = new P08[Int]
  assert(p08.compress(List()) == List())
  assert(p08.compress(List(1, 1)) == List(1))
  assert(p08.compress(List(1, 2, 3, 5, 5, 5, 6)) == List(1, 2, 3, 5, 6))
}
