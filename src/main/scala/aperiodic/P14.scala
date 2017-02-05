package aperiodic

class P14[A] {
  def duplicate(list: List[A]): List[A] = list flatMap (element => List(element, element))
}

object P14 extends App {
  val p14 = new P14[Symbol]
  assert(p14.duplicate(List()) == List())
  assert(p14.duplicate(List('a)) == List('a, 'a))
  assert(p14.duplicate(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
}
