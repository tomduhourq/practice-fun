package aperiodic

import scala.annotation.tailrec

class P13[A] {
  def encodeDirect(list: List[A]): List[(Int, A)] = {
    @tailrec
    def encodeDirectRec(partial: List[(Int, A)], howMany: Int, current: A, left: List[A]): List[(Int, A)] = left match {
      case Nil => partial :+ (howMany, current)
      case head :: tail if head == current => encodeDirectRec(partial, howMany + 1, current, tail)
      case head :: tail => encodeDirectRec(partial :+ (howMany, current), 1, head, tail)
    }
    if (list.isEmpty) Nil
    else {
      val head = list.head
      encodeDirectRec(Nil, 1, head, list.tail)
    }
  }
}

object P13 extends App {
  val p13 = new P13[Symbol]
  assert(p13.encodeDirect(List()) == List())
  assert(p13.encodeDirect(List('a)) == List((1, 'a)))
  assert(p13.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
}
