val primes: Stream[Int] = 2 #::
  Stream.from(3,2).filter{i => primes
    .takeWhile(j => j*j <= i).forall(i % _ != 0)}

def gcd(a: Int, b: Int): Int=
  if(b == 0) a else gcd(b, a % b)

object Utils {
  implicit class RichInt(val v: Int) {
    def isPrime = v % 2 != 0 && primes.take(v).contains(v)
    def isCoprimeTo(a: Int) = gcd(v, a) == 1
    lazy val totient =
      if(v % 2 == 0) v / 2
      else
        (1 to v).count(v.isCoprimeTo)
    lazy val primeFactors =
      (1 to v)
        .filter(x => x.isPrime && v % x == 0)
  }
}

import Utils.RichInt

315.primeFactors