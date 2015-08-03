//Implement several solutions to common problems using folds
val nums = List(1,2,3)

// SUM
nums.foldLeft(0)((partialRes, num) => partialRes + num)
nums.foldRight(0)((num, partialRes) => num + partialRes)

// REVERSE -> Consequent prepending of the head
nums.foldLeft(Nil: List[Int])((partialRes, num) => num :: partialRes)

// LENGTH -> As long as there are elements in the list, sum 1 to the res.
nums.foldLeft(0)((partialResult, _) => partialResult + 1)

// AVERAGE -> we can reuse count and sum or make both in one
nums.foldRight(0)((num, partialRes) => num + partialRes) /
  nums.foldLeft(0)((partialResult, _) => partialResult + 1)

val (sum, count) =
  nums.foldLeft((0, 0))((acc, elem) => (acc._1 + elem, acc._2 + 1))
sum / count

// LAST
nums.foldLeft(nums head)((_, c) => c)
nums.foldLeft(Nil: List[Int])((partialRes, num) => num :: partialRes) head

// PENULTIMATE
nums.foldLeft( (nums.head, nums.tail.head) )((r, c) => (r._2, c) )._1

// CONTAINS
def contains[A](xs: List[A], elem: A) =
  xs.foldLeft(false)((p, e) => p || e == elem)

// toSet
def toSet[A](xs: List[A]) = xs.foldLeft(Set[A]())((r, c) => r + c)

// Double -> Return 2 times the list consequently
nums.foldRight(Nil: List[Int])((elem, p) => elem :: elem :: p)
nums.foldLeft(Nil: List[Int])((p, elem) => elem :: elem :: p) reverse


def foldLeft[A,B](xs: List[A])(z: B)(f: (B, A) => B): B = {
  def loop(l:List[A], acc: B, ff: (B, A) => B):B = {
    if (!l.isEmpty) loop(l.tail, ff(acc, l.head), ff)
    else acc
  }

  loop(xs, z, f)
}