package aperiodic

object P04length {
  /* Find the number of elements of a list.
Example:
scala> length(List(1, 1, 2, 3, 5, 8))
res0: Int = 6*/

	def length(xs :List[Any]):Int = xs match{
		case n :: Nil => 1
		case n :: tail => 1 + length(tail)
		case Nil => 0
	}                                         //> length: (xs: List[Any])Int
	
	length(List(0,1,2))                       //> res0: Int = 3
	length(List())                            //> res1: Int = 0
	length(List(123,123,2,"asf"))             //> res2: Int = 4
}