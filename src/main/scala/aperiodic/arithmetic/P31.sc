val primes: Stream[Int] = 2 #::
  Stream.from(3,2).filter{i => primes
    .takeWhile(j => j*j <= i).forall(i % _ != 0)}

object Utils {
  implicit class RichInt(val n: Int) {
    def isPrime = if(n % 2 == 0) false
    else primes.take(n).contains(n)
  }
}

import Utils.RichInt

61.isPrime