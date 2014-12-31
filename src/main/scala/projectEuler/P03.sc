package projectEuler
import mathematics.Mathematics
object P03 {
  /*The prime factors of 13195 are 5, 7, 13 and 29.
	What is the largest prime factor of the number 600851475143 ?
	*/
  val primeSource = new Mathematics               //> primeSource  : mathematics.Mathematics = mathematics.Mathematics@3b95a09c
  val n = 600851475143L                           //> n  : Long = 600851475143
  primeSource.primes.takeWhile(Math.sqrt(n)>).filter(n % _ == 0).max
                                                  //> res0: Int = 6857
}