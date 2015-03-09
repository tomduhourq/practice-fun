package projectEuler
import mathematics.Mathematics
object P10 {
  /*The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
		Find the sum of all the primes below two million.*/

  // If I just put sum after takeWhile we would be having an incorrect answer due to scala.Int's upper bound.
  Mathematics.primes.takeWhile(2000000>).foldLeft(0L)(_ + _)
  //> res0: Long = 142913828922

}