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
  }
}
