package codility.lesson02

/**
 * Correctness: 80%
 * Performance: 80%
 */
object Permutation {
  // Determine whether an Array of N elements is a permutation:
  // it contains exactly once each element in Range(1,N).inclusive
  def solution(A: Array[Int]): Int = {
    val missing = A.max
    var totalSum = missing*(missing + 1)/2
    val flags = Array.fill[Boolean](missing + 1)(false)
    for (i <- A){
      if(!flags(i - 1)){
        flags(i - 1) = true
        totalSum -= i
      }
      // repeated number
      else return 0
    }
    // not all numbers were included
    if(totalSum > 0) 0 else 1
  }
}
