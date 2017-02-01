package aperiodic

import scala.annotation.tailrec

object P07 extends App {
  def flatten(list: List[Any]): List[Any] = {
    @tailrec
    def flattenRec(partial: List[Any], left: List[Any]): List[Any] = left match {
      case Nil => partial
      case ::(head: List[Any], tail) => flattenRec(partial ::: flatten(head), tail)
      case head :: tail => flattenRec(partial :+ head, tail)
    }
    flattenRec(Nil, list)
  }

  assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
  assert(flatten(List(List(List(List())))) == List())
}
