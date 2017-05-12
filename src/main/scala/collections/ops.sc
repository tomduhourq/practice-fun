import performance.Perf

import scala.annotation.tailrec
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


def takeFirstN[A](n: Int, p: A => Boolean, list: List[A]): List[A] = {
  @tailrec
  def takeRec(left: Int, leftList: List[A], partialResult: List[A]): List[A] = leftList match {
    case Nil => partialResult
    case elem :: _ if p(elem) && left == 1 => partialResult :+ elem
    case elem :: tail if p(elem) && left > 1 => takeRec(left - 1, tail, partialResult :+ elem)
    case _ :: tail => takeRec(left, tail, partialResult)
  }
  takeRec(n, list, Nil)
}

takeFirstN[Int](2, x => x > 2, List(1, 2, 2, 3, 2, 4))