// Insert an element at a given position
def insertAt[A](e: A, pos: Int, l: List[A]) = {
  def insertTailRec(missing: Int, xs: List[A]): List[A] =
    (missing, xs) match {
      case (0, x :: xs) => e :: x :: xs
      case (n, x :: xs) => x :: insertTailRec(n - 1, xs)
    }
  insertTailRec(pos, l)
}

insertAt('new, 1, List('a, 'b, 'c, 'd))