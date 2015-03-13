// Split a List at a given index
def split[A](n: Int, l: List[A]): Option[(List[A],List[A])] =
  if(l.length < n) None
  else Some((l.take(n),l.drop(n)))

