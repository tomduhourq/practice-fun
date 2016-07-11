// Given a range of integers by its lower and upper limit,
// construct a list of all prime numbers in that range.
val primes: Stream[Int] = 2 #::
  Stream.from(3,2).filter { i =>
    primes
      .takeWhile(j => j * j <= i)
      .forall(i % _ != 0)
  }

def listPrimesInRange(r: Range) =
  primes
    .dropWhile(_ < r.head)
    .takeWhile(_ <= r.last)
    .toList

listPrimesInRange (7 to 31)

