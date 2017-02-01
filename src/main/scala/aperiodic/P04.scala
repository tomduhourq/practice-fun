package aperiodic

class P04[A] {
  def length(list: List[A]): Int = list.foldLeft(0)((count, _) => count + 1)
}

object P04 extends App {
  val p04 = new P04[Int]
  assert(p04.length(List(1,2,3)) == 3)
  assert(p04.length(List()) == 0)
  assert(p04.length(List(1)) == 1)
}
