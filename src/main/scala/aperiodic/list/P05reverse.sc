package aperiodic

object P05reverse {
  /*P05 (*) Reverse a list.
Example:
scala> reverse(List(1, 1, 2, 3, 5, 8))
res0: List[Int] = List(8, 5, 3, 2, 1, 1)*/
	def reverse(xs: List[Any]): List[Any] = xs match {
		case Nil => Nil
		case h :: tail => reverse(tail) ::: List(h)
	}                                         //> reverse: (xs: List[Any])List[Any]
	
	reverse(List(1,2,3))                      //> res0: List[Any] = List(3, 2, 1)
	reverse(List("apple","pear","banana"))    //> res1: List[Any] = List(banana, pear, apple)
}