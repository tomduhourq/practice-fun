package projectEuler
import mathematics.Mathematics
object P07 {
  /*By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
	What is the 10 001st prime number?*/
	
	// I'll use the Mathematics class
	val primeSource = new Mathematics         //> primeSource  : mathematics.Mathematics = mathematics.Mathematics@3b95a09c
	// Zero indexed Stream
	primeSource.primes(10001 - 1)             //> res0: Int = 104743
}