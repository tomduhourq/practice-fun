package aperiodic

class P06[A](p05: P05[A]) {
  def isPalindrome(list: List[A]): Boolean = p05.reverse(list) == list
}

object P06 extends App {
  val p05 = new P05[Int]
  val p06 = new P06(p05)
  assert(p06.isPalindrome(List(1, 2, 3, 2, 1)))
  assert(!p06.isPalindrome(List(1, 1, 2, 3, 5, 8)))
  assert(p06.isPalindrome(List()))
}
