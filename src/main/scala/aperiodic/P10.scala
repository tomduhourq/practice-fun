package aperiodic

class P10[A] extends P09[A] {
  def encode(list: List[A]): List[(Int, A)] = pack(list) map (packed => (packed.length, packed.head))
}

object P10 extends App {
  val p10 = new P10[Symbol]
  assert(p10.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  assert(p10.encode(List()) == List())
  assert(p10.encode(List('a)) == List((1, 'a)))
}
