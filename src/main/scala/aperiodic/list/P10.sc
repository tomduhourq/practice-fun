def encode[A](l: List[A]): List[(Int,A)] = {
  def encodeRec[A](actual: A, acum: Int, xs: List[A]): (Int, A) =
    xs match {
      case Nil =>
        (acum, actual)
      case h :: t if xs.head != actual =>
        (acum, actual)
      case h :: t if xs.head == actual =>
        encodeRec(actual, acum + 1, xs.drop(1))
  }
  l match {
    case Nil => Nil
    case h :: t =>
      encodeRec(h, 1, t) ::
        encode(l.dropWhile(h==))
  }
}

encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))