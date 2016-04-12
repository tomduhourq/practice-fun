// give the sum of the digit which is more common between 0 and k
def mostCommonDigitSum(k: Int) = {
  val sum = (0 to k) map (_.toString
    .map(_.toString.toInt).foldLeft(0)((acum, elem) => acum + elem))
  sum.groupBy(identity)
    .map{ case (sum, qty) => sum -> qty.length}
      .maxBy(_._2)._2
}

mostCommonDigitSum(10)
mostCommonDigitSum(50)
mostCommonDigitSum(7777)


// Find two prime numbers that sum up to n
// and return the lower number

lazy val primes0: Stream[Int] = 2 #::
  Stream.from(3,2).filter{i => primes0
    .takeWhile(j => j*j <= i).forall(i % _ != 0)}

def twoPrimesThatSumUpTo(n: Int) =
  (for {
    prime <- primes0.takeWhile(n >)
    prime2 <- primes0.takeWhile(prime >)
    if prime + prime2 == n
  } yield (prime, prime2)).toList

twoPrimesThatSumUpTo(50)