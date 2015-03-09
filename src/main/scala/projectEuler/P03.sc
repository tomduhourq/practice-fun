package projectEuler
import mathematics.Mathematics
object P03 {
  /*The prime factors of 13195 are 5, 7, 13 and 29.
	What is the largest prime factor of the number 600851475143 ?
	*/
  val n = 600851475143L
  Mathematics.primes.takeWhile(Math.sqrt(n)>).filter(n % _ == 0).max
  // 6857
}