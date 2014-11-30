package aperiodic

object P07flatten {
/*	Flatten a nested list structure.
Example:
scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
res0: List[Any] = List(1, 1, 2, 3, 5, 8)*/
	def flat(l: List[Any]): List[Any] = l match {
		case Nil => Nil
		case x :: Nil => List(x)
		case x :: xs => (x match {
			case h :: Nil => List(h)
			case list: List[Any] => flat(list)
		}) ::: flat(xs)
	}                                         //> flat: (l: List[Any])List[Any]
	
  flat(List(List(1,2,3),Nil,List(List(4))))       //> scala.MatchError: 1 (of class java.lang.Integer)
                                                  //| 	at aperiodic.P07flatten$$anonfun$main$1.flat$1(aperiodic.P07flatten.scal
                                                  //| a:11)
                                                  //| 	at aperiodic.P07flatten$$anonfun$main$1.flat$1(aperiodic.P07flatten.scal
                                                  //| a:13)
                                                  //| 	at aperiodic.P07flatten$$anonfun$main$1.apply$mcV$sp(aperiodic.P07flatte
                                                  //| n.scala:17)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at aperiodic.P07flatten$.main(aperiodic.P07flatten.scala:3)
                                                  //| 	at aperiodic.P07flatten.main(aperiodic.P07flatten.scala)
}