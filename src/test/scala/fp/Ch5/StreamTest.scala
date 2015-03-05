package fp.Ch5

import org.scalatest.{FreeSpec, Matchers}


/**
 * Created by tomas on 25/02/15.
 */
class StreamTest extends FreeSpec with Matchers {
  "My Stream trait" - {
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
    "applies takeWhileByFoldRight" in {
      s.takeWhileByFoldRight(124>).toList should be (List(1,2,123))
      s.takeWhileByFoldRight(12>).toList should be (List(1,2))
    }
    "maps correctly + 1" in {
      s.map(_ + 1).toList should be (List(2,3,124))
    }
    "filters correctly" in {
      s.filter(pFalse).toList should be (List(1))
    }
    "appends" in {
      s.append(Stream.cons(5,Empty)).toList should be (List(1,2,123,5))
    }
    "flatMaps everyone to String" in {
      s.flatMap( x => Stream(x.toString)).toList should be (List("1","2","123"))
    }
  }

  "My Stream companion object" - {

    "gives 5 ones" in {
      Stream.ones.take(5).toList should be (List(1,1,1,1,1))
    }
    "from 23 and taking 5" in {
      Stream.from(23).take(5).toList should be (List(23,24,25,26,27))
    }
    "gets first 6 fibs" in {
      Stream.fibs.take(6).toList should be(List(0, 1, 1, 2, 3, 5))
    }

  }
}
