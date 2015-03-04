package fp.Ch5

import org.scalatest.{FreeSpec, Matchers}

/**
 * Created by tomas on 25/02/15.
 */
class StreamTest extends FreeSpec with Matchers {
  "My own Stream" - {
    val s = Stream.cons(1,Stream.cons(2,Stream.cons(123,Empty)))

    "parses to a List correctly" in {
      s.toList should be (List(1,2,123))
    }

    "takes 2" in {
      s.take(2).toList should be (List(1,2))
    }

    "takes while 124 is greater" in {
      s.takeWhile(124>).toList should be (List(1,2,123))
    }

    val pFalse = (x: Int) => x < 2
    val pTrue = (x: Int) => x >= 1
    "applies forAll correctly" in {
      s.forAll(pFalse) should be (false)
      s.forAll(pTrue) should be (true)
    }
  }
}
