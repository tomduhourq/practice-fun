package fp

import org.scalatest.FlatSpec

/**
 * Created by tomas on 11/02/15.
 */
class Ch3Test extends FlatSpec {
  private val fp = new Ch3()

  behavior of "tail"
  it should "Nil" in {
    assert(fp.tail(Nil) === Nil)
  }
}
