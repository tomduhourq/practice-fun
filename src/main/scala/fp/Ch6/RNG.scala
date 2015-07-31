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
    val seed = rng.nextInt
    (determinePositive(seed _1), seed _2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val seed = rng.nextInt
    (seed._1 - Math.floor(seed._1), seed _2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r) = rng.nextInt
    val d = double(r)._1
    ((i, d), r)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val rngs = for(_ <- 1 to count) yield rng.nextInt
    (rngs map (_._1) toList, rngs(0)._2)
  }

  private def determinePositive(seed: Int) = if(seed < 0) -(seed + 1) else seed
}
