package aperiodic

object P02penultimate {
/*P02 (*) Find the last but one element of a list.
Example:
scala> penultimate(List(1, 1, 2, 3, 5, 8))
res0: Int = 5 */
  def penultimate(xs: List[Any]): Any = xs match {
  	case x :: _ :: Nil => x
  	case _ :: tail => penultimate(tail)
  	case _ => Option("No penultimate element")
  }                                               //> penultimate: (xs: List[Any])Any
  
  penultimate(List(1,2,123,3))                    //> res0: Any = 123
  penultimate(List("apple","orange"))             //> res1: Any = apple
}