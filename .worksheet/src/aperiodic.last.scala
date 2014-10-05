package aperiodic
object last {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(296); 
// P01 (*) Find the last element of a list.
//     Example:
//     scala> last(List(1, 1, 2, 3, 5, 8))
//     res0: Int = 8
  def last(xs: List[Any]): Any = xs match {
    case n :: Nil => n
    case x :: tail => last(tail)
    case _ => Option("Invalid list")
  };System.out.println("""last: (xs: List[Any])Any""");$skip(22); val res$0 = 
  last(List(1,2,3,4));System.out.println("""res0: Any = """ + $show(res$0))}
}
