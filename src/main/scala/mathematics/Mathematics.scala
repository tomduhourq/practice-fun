package mathematics

class Mathematics {

  def isPrime(n: Int): Boolean = {
    var i = 2
    // Check up to square root of the number
    while (i * i <= n && n % i != 0) i += 1
    n % i != 0
  }

  lazy val primes: Stream[Int] = 2 #:: 
    Stream.from(3, 2).filter(isPrime)
  // This one determines 'in line' if the number to add is prime.
  lazy val primes2: Stream[Int] = 2 #:: primes2.map(i =>
    Stream.from(i + 1).find(j => primes2.takeWhile(k => k * k <= j).forall(j % _ > 0)).get)
  // Best implementation I could reach
  lazy val primes0: Stream[Int] = 2 #:: 
    Stream.from(3,2).filter{i => primes.takeWhile(j => j*j <= i).forall(i % _ != 0)}    
    
  def firstNPrimes(n: Int) = primes0.takeWhile(n>).toSet
}
