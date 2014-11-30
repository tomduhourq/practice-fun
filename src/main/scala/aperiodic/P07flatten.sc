package aperiodic

object P07flatten {
/*	Flatten a nested list structure.
Example:
scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
res0: List[Any] = List(1, 1, 2, 3, 5, 8)*/
	  def flat(ls: List[Any]): List[Any] = ls flatMap {
    case x : List[_] => flat(x)
    case e => List(e)
  }                                               //> flat: (ls: List[Any])List[Any]
	
  flat(List(List(1,2,3)))                         //> res0: List[Any] = List(1, 2, 3)
}