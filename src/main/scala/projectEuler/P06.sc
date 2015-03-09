package projectEuler

object P06 {
  /*The sum of the squares of the first ten natural numbers is,
	12 + 22 + ... + 102 = 385
	The square of the sum of the first ten natural numbers is,
	(1 + 2 + ... + 10)2 = 552 = 3025
	Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
	Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.*/
  val range = Range(1,100).inclusive
  def square(x:Int) = x*x
  val sumOfRange = 100*101/2
  val squareSumOfRange = square(sumOfRange)
  //> squareSumOfRange  : Int = 25502500
  // The result must be given in absolute value.
  range.map(square).sum - squareSumOfRange // -25164150

}
