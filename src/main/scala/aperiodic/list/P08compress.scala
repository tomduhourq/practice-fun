package aperiodic

object P08compress {
  /* Eliminate consecutive duplicates of list elements.
     If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
     Example:

     scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
     res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e) */

  def compress(l: List[Any]):	List[Any] = l match {
    case Nil => Nil
    case x :: xs => x :: compress(xs dropWhile(_ == x))
  }                                 //> compress: (l: List[Any])List[Any]

  compress(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
  //> res0: List[Any] = List(a, b, c, a, d, e)
}