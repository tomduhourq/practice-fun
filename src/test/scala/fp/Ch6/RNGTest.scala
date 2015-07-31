package fp.Ch6

import org.scalacheck._
import Prop._

class RNGTest extends Properties("RNG") {

  lazy val genRNG: Gen[RNG] = for {
    n <- Gen.choose(Int.MinValue, Int.MaxValue)
  } yield RNG.simple(n)

  implicit lazy val arbRNG: Arbitrary[RNG] = Arbitrary(genRNG)

  property("Abs always positive") = forAll {
    (x: RNG) => RNG.positiveInt(x)._1 >= 0
  }

  property("Double always greater or equal to 0 and less than 1") = forAll {
    (x: RNG) =>
      val d = RNG.double(x)._1
      d >= 0 && d < 1
  }
}
