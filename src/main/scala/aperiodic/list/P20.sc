// Remove the kth element from a list and return it as a tuple
def removeAt[A](k: Int, l: List[A]): Option[(List[A], A)] = {
  def removeRec(left: Int, actualList: List[A]): List[A] = (left, actualList) match {
    case (0, x :: xs) => xs ::: List(x)
    case (n, x :: xs) if n > 0 => x :: removeRec(n - 1, xs)
  }
  val size = l.length
  if (k > size) None
  else {
    val list = removeRec(k, l)
    Some(list.take(size - 1), list(size - 1))
  }
}