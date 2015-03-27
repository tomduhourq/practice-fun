// Rotate a List n places to the left, or n to the right if n < 0
def rotate[A](n: Int, l: List[A]): List[A] = n match {
  case x if x > 0 =>
    l.drop(n) ::: l.take(n)
  case x if x < 0 =>
    l.takeRight(-n) ::: l.dropRight(-n)
  case _ => l
}
