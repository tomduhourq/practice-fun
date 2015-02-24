package fp

import fp.Ch3.Lists
import org.scalatest.FlatSpec

/**
 * Created by tomas on 11/02/15.
 */
class Ch3Test extends FlatSpec {
  private val fp = new Lists

  behavior of "tail"
  it should "Nil" in {
    assert(fp.tail(Nil) === Nil)
  }

  behavior of "length"
  it should "0" in {
    assert(fp.length(Nil) === 0)
  }

  it should "10" in {
    assert(fp.length(List(1,2,3,4,5,6,7,8,9,10)) === 10)
  }
}
