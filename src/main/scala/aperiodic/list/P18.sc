// Extract a slice from the given List, starting by 0
def possibleFailure[A](st: Int, end: Int, l: List[A]) =
  end - st < 0 || st < 0 || end < 0 || l.isEmpty

def slice[A](st: Int, end: Int, l: List[A]): Option[List[A]] =
  if(possibleFailure(st, end, l)) None
  else Some(l.drop(st).take(end - st))

