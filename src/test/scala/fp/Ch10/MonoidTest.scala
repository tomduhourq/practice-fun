package fp.Ch10

import org.scalatest.{FreeSpec, Matchers}

class MonoidTest extends FreeSpec with Matchers {
  "My monoid" - {
    val l = List(1,2,3,4,5)
    "FoldMaps Ints to Strings" in {
      Monoid.foldMap(l, Monoid.stringMonoid)(_.toString) should be ("12345")
    }
  }
}
