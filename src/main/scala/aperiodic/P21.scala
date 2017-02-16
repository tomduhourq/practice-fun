package aperiodic

class P21[A] {
  def insertAt(element: A, at: Int, list: List[A]): List[A] = {
    val length = list.length
    if (at < 0 || at >= length) list
    else (list.take(at) :+ element) ++ list.takeRight(length - at)
  }
}

object P21 extends App {
  val p21 = new P21[Symbol]
  assert(p21.insertAt('a, -1, List('b, 'c)) == List('b, 'c))
  assert(p21.insertAt('a, 2, List('b, 'c)) == List('b, 'c))
  assert(p21.insertAt('a, 3, List('b, 'c)) == List('b, 'c))
  assert(p21.insertAt('new, 1, List('a, 'b, 'c, 'd)) == List('a, 'new, 'b, 'c, 'd))
}
