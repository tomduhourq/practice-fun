package aperiodic

class P06[A] extends P05[A] {
  def isPalindrome(list: List[A]): Boolean = reverse(list) == list
}

object P06 extends App {
  val p06 = new P06[Int]
  assert(p06.isPalindrome(List(1, 2, 3, 2, 1)))
  assert(!p06.isPalindrome(List(1, 1, 2, 3, 5, 8)))
  assert(p06.isPalindrome(List()))
}
