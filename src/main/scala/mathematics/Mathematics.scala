package mathematics
import NumUtils._

object Mathematics {

  def binomial(n: Int, k: Int) = (n!) / ((k!) * ((n - k)!))
  lazy val fibs: Stream[BigInt] = 0 #:: 1 #::
    fibs.zip(fibs.tail).map(p => p._1 + p._2)
  lazy val fibs2: Stream[BigInt] = 0 #::
    fibs2.scanLeft(BigInt(1)){_ + _}
  lazy val fibs3: Stream[Long] = {
    def fibRec(a: Long, b: Long): Stream[Long] = a #:: fibRec(b, a + b)
    fibRec(0, 1)
  }
  lazy val primes: Stream[Int] = 2 #::
    Stream.from(3, 2).filter(_.isPrime)
  // This one determines 'in line' if the number to add is prime.
  lazy val primes2: Stream[Int] = 2 #:: primes2.map(i =>
    Stream.from(i + 1).find(j => primes2
      .takeWhile(k => k * k <= j).forall(j % _ > 0)).get)
  lazy val primes3 = sieve(Stream.from(2))
  // Best implementation I could reach
  lazy val primes0: Stream[Int] = 2 #:: 
    Stream.from(3,2).filter{i => primes0
      .takeWhile(j => j*j <= i).forall(i % _ != 0)}
  // Yet another tail Rec implementation
  val primesTailRec: Stream[Int] = {
    def generatePrimes (s: Stream[Int]): Stream[Int] =
      s.head #:: generatePrimes(s.tail filter (_ % s.head != 0))
    generatePrimes(Stream.from(2))
  }
  // Sieve of Erathosthenes
  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail.filter(_ % s.head != 0))

  def firstNPrimes(n: Int) = primes0.takeWhile(n>).toList
}
