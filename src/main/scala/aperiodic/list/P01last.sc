package aperiodic
object last {
// P01 (*) Find the last element of a list.
//     Example:
//     scala> last(List(1, 1, 2, 3, 5, 8))
//     res0: Int = 8
  def last(xs: List[Any]): Any = xs match {
    case n :: Nil => n
    case x :: tail => last(tail)
    case _ => Option("Invalid list")
  }                                               //> last: (xs: List[Any])Any
  last(List(1,2,3,4))                             //> res0: Any = 4
}