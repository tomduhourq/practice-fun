// Duplicate the elements of a List
def duplicate[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case h :: t => h :: h :: duplicate(t)
}

// Do the same n times
def duplicateN[A](n: Int, l: List[A]): List[A] = {
  def generator(left: Int, elem: A): List[A] =
    if(left == 0) Nil
    else elem :: generator(left - 1, elem)
  l.flatMap(x => generator(n, x))
}

