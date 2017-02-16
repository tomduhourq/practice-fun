package aperiodic

class P20[A] {
  def removeAt(n: Int, list: List[A]): (List[A], Option[A]) = {
    val length = list.length
    if (n > length - 1 || n <= 0) (list, None)
    else (list.take(n) ::: list.takeRight(length - n - 1), Some(list(n)))
  }
}

object P20 extends App {
  val p20 = new P20[Symbol]
  assert(p20.removeAt(1, List('a, 'b, 'c, 'd)) == (List('a, 'c, 'd), Some('b)))
  assert(p20.removeAt(4, List('a, 'b, 'c, 'd)) == (List('a, 'b, 'c, 'd), None))
  assert(p20.removeAt(-4, List('a, 'b, 'c, 'd)) == (List('a, 'b, 'c, 'd), None))
}
