package fp

import org.scalatest.FlatSpec
import org.junit.Before

class Ch2Test extends FlatSpec {
  private val fp = new Ch2()

  behavior of "fib"
  it should "be 0" in {
    assert(fp.fib(0) == 0)
  }

  it should "be 3" in {
    assert(fp.fib(4) == 3)
  }

  it should "be 17711" in {
    assert(fp.fib(22) == 17711)
  }

  "Array(1,2,3,4) with ordering minor-than" should "raise true" in {
    assert(fp.isSorted(Array(1, 2, 3, 4), (x: Int, y: Int) => x < y))
  }

  "Array(1,3,2,4) with ordering minor-than" should "raise false" in {
    assert(!fp.isSorted(Array(1, 3, 2, 4), (x: Int, y: Int) => x < y))
  }

  behavior of "partial1 using underscore notation"
  def f2(a: Int, b: Int) = a + b
  it should "return a function with one parameter" in {
    val f1: (Int) => Int = fp.partial1(3, f2)
    assert(f1(10) === 13)
  }

}