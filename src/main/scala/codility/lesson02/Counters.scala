package codility.lesson02

/**
 * Correctness: 75%
 * Performance: 20% lulz
 * Created by tomasduhourq on 2/25/15.
 */
object Counters {
  def solution(N: Int, A: Array[Int]): Array[Int] = {
    // 'a' is the position we must (or not) update
    def applyFunction(counters: Array[Int], a: Int): Array[Int] =
      if(N - a >= 0) {counters.update(a - 1, counters(a - 1) + 1); counters}
      else Array.fill[Int](N)(counters.max)
    def sol(counters: Array[Int], missing: Array[Int]): Array[Int] = {
      if(missing.isEmpty) counters
      else sol(applyFunction(counters, missing(0)), missing.drop(1))
    }
    sol(Array.fill[Int](N + 1)(0), A)
  }
}
