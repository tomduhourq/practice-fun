import performance.Perf

import scala.util.Random

// Get the intersection of both arrays
def intersect(arr1: Array[Int], arr2: Array[Int]): Array[Int] = {
  val (_, intersection) = arr2.foldLeft((arr1.toSet, Array.empty[Int])){
    case ((set, result), elem) =>
    if (set.contains(elem)) (set - elem, result :+ elem)
    else                    (set, result)
  }
  intersection
}

intersect(Array(1), Array())
intersect(Array(1,1,1,2,3), Array(2, 3, 4))

val rand = new Random
val constant = Stream.continually((rand.nextInt + 10) * (4000 - 10))

Perf.time(intersect(constant.take(2000).toArray, constant.take(1000).toArray))