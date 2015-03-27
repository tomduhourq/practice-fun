// Drop every nth element of a List
def drop[A](n: Int, l: List[A]): List[A] = l match {
  case x :: xs if l.length >= n - 1 =>
    l.take(n - 1) ::: drop(n, l.drop(n))
  case _ => Nil
}
