package codility.lesson02

/**
 * Correctness: 100%
 * Performance: 100%
 * Created by tomasduhourq on 2/25/15.
 */
object FrogRiverOne {

  def solution(X: Int, A: Array[Int]): Int = {
    val counter = Array.fill[Boolean](X + 1)(false)
    var x = X
    for (i <- 0 until A.length) {
      val value = A(i)
      //--I didn't find the number till now
      if (!counter(value)) {
        counter(value) = true
        x -= 1
      }
      if (x == 0)
        return i
    }
    -1
  }                                               //> solution: (X: Int, A: Array[Int])Int

  solution(5, Array(1, 3, 1, 4, 2, 3, 5))         //> res0: Int = 6
}