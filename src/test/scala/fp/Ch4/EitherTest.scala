package fp.Ch4

import org.scalatest.{Matchers, FreeSpec}
import fp.Ch4.Either

/**
 * Created by tomas on 24/02/15.
 */
class EitherTest extends FreeSpec with Matchers {
  "My Either trait" - {
    val eitherString = Left("this is a representation of an error")
    val eitherInt = Right(3)
    val eitherInt2 = Right(4)

    val g = (x: Int) => 4 * x
    val f = (x: Int,y: Int) => x + y
    "throws a representation of an error when calling wrongly map" in {
      eitherString.map(g) should be (eitherString)
    }

    "throws Right(12) when calling the same map function" in {
      eitherInt.map(g) should be (Right(12))
    }

    "throws Right(4 + 3) when calling map2" in {
      eitherInt.map2(eitherInt2)(f) should be (Right(7))
    }

    "throws eitherString when calling map2 inappropiately" in {
      eitherInt.map2(eitherString)(f) should be (eitherString)
    }
  }
}
