/**
  If number a is a divisor of n, then n/a is also a divisor.
  One of these two divisors is less than or equal to √n.
  (If that were not the case, n would be
  a product of two numbers greater than √n, which is impossible.)
 */
def numberOfDivisors(n: Int): Int =
  Range(1, Int.MaxValue)
  .takeWhile(i => i * i <= n)
  .foldLeft(0)((total, a) => if(n % a == 0) total + 2 else total)

lazy val triangles: Stream[Int] =
  Stream.from(1).map( n => n*(n+1)/2)

triangles.find(numberOfDivisors(_) > 500)
// 76576500
