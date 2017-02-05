package aperiodic

class P12[A] {
  def decode(list: List[(Int, A)]): List[A] = list.foldLeft(Nil:List[A]){
    case (currentList, (howMany, element)) => currentList ::: List.fill(howMany)(element)
  }
}

object P12 extends App {
  val p12 = new P12[Symbol]
  assert(p12.decode(List()) == List())
  assert(p12.decode(List((1, 'a), (3, 'b))) == List('a, 'b, 'b, 'b))
  assert(p12.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
}
