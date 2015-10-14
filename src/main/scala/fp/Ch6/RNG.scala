package fp.Ch6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    (determinePositive(n), rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = rng.nextInt
    (n - Math.floor(n), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val d = double(r)._1
    ((i, d), r)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val rngs = for(_ <- 1 to count) yield rng.nextInt
    (rngs map (_._1) toList, rngs(0)._2)
  }

  type Rand[+A] = RNG => (A, RNG)

  // Common combinators
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  val int: Rand[Int] = _ nextInt

  // Exercise 5 --> Use map to generate an Int between 0 and n.
  def positiveMax(n: Int): Rand[Int] = map(positiveInt)(_ % n)

  private def determinePositive(seed: Int) = if(seed < 0) -(seed + 1) else seed
}
