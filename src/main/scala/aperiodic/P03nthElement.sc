package aperiodic

object P03nthElement {
  /*P03 (*) Find the Kth element of a list.
By convention, the first element in the list is element 0.
Example:
scala> nth(2, List(1, 1, 2, 3, 5, 8))
res0: Int = 2*/
	def nth(n: Int, xs: List[Any]): Any = {
		require(n < xs.size)
		xs(n)
	}                                         //> nth: (n: Int, xs: List[Any])Any
	
	nth(2,List(1,2,3,4,5))                    //> res0: Any = 3
}