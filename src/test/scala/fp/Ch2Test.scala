package fp

import org.scalatest.FlatSpec

class Ch2Test extends FlatSpec {
  import Ch2._

  val gt = (x: Int, y: Int) => x >= y
  val lt = (x: Int, y: Int) => x < y

  behavior of "fib"
  it should "be 0" in {
    assert(fib(0) == 0)
  }

  it should "be 3" in {
    assert(fib(4) == 3)
  }

  it should "be 17711" in {
    assert(fib(22) == 17711)
  }

  Set((1, isSorted[Int](_, _)), (2, isSorted2[Int](_, _))) foreach { case (n, f) =>
    s"Array(1,2,3,4) with ordering minor-than for function sorted $n" should "raise true" in {
      assert(f(Array(1, 2, 3, 4), lt))
    }

    s"Array(1,3,2,4) with ordering minor-than for function sorted $n" should "raise false" in {
      assert(!f(Array(1, 3, 2, 4), lt))
    }

    s"Array(1,25,236,3) with ordering gt for function sorted $n" should "raise false" in {
      assert(!f(Array(1, 25, 236, 3), gt))
    }
  }

  behavior of "partial1"
  def f2(a: Int, b: Int) = a + b
  it should "return a function with one parameter" in {
    val f1: Int => Int = partial1(3, f2)
    assert(f1(10) === 13)
  }

  behavior of "curry"
  it should "return a function that takes its parameters one by one" in {
    assert(curry((x:Int,y:Int) => x*y)(10)(2) === 20)
  }
}
