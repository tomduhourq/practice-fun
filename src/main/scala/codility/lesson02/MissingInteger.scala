package codility.lesson02

import scala.collection.immutable.SortedSet

/**
 * Correctness: 100%
 * Performance: 100%
 */
object MissingInteger {
  // Find the minimum missing number that doesn't occur in A
  def solution(A: Array[Int]): Int =
    // Consequent subtractions of the SortedSet of A (my seed would be the set)
    A.foldLeft( SortedSet( (1 to (A.length + 1)).toSeq: _*) )(_ - _).headOption.getOrElse(1)
}
