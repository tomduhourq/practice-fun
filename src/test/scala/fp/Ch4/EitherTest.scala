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

    "throws a representation of an error when calling wrongly map" in {
      eitherString.map((x:Int) => 4 * x) should be (eitherString)
    }

    "throws Right(12) when calling the same map function" in {
      eitherInt.map((x:Int) => 4 * x) should be (Right(12))
    }
  }
}
