package projectEuler

object P02 {
  /* Each new term in the Fibonacci sequence is generated by adding
  the previous two terms. By starting with 1 and 2, the first 10 terms will be:
1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
By considering the terms in the Fibonacci sequence whose values do not exceed four million,
 find the sum of the even-valued terms.*/

  //--This is the regular definition of a fib sequence (very high cost)
  def fibonacciSlow(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case x => fibonacciSlow(x - 1) + fibonacciSlow(x - 2)
  }                                               //> fibonacciSlow: (n: Int)Int

  //--This is another much faster way taken from
  //--http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.Stream
  lazy val fibs: Stream[BigInt] = 0 #:: 1 #:: fibs.zip(fibs.tail).map(p => p._1 + p._2)
  //> fibs: => Stream[BigInt]
  def sumOfEvenFibsUpTo(n: Int) = fibs.takeWhile(_ <= n).filter(p => p % 2 == 0).sum
  //> sumOfEvenFibsUpTo: (n: Int)BigInt
  sumOfEvenFibsUpTo(4000000)                      //> res0: BigInt = 4613732

}