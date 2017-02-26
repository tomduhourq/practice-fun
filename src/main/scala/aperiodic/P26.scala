package aperiodic

class P26[A] {
  def combinations(by: Int, list: List[A]): List[List[A]] = list.permutations.toList.flatten.grouped(by).toList
}

object P26 extends App {
  val p26 = new P26[Symbol]
  val combinations = p26.combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
  println(combinations.length)
  println(combinations)
}
