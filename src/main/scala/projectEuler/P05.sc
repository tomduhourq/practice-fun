package projectEuler

object P05 {
  /*2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
	What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?*/

  // Tried to solve it using a Stream.from(1), got an OutOfMemoryError, so instead I used Range
  Range(20, Int.MaxValue).find(n => Range(2, 20).inclusive.forall(n % _ == 0))
  //> res0: Option[Int] = Some(232792560)
}