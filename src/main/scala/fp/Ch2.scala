package fp

import scala.Function1

// This class represents Chapter 2 exercises of Functional Programming in Scala
class Ch2 {
  // Exercise p.22 --> Provide the nth Fibonacci number with a tail recursive function
  def fib(n: Int): Int = {
    def fibRecursive(first: Int, second: Int, count: Int): Int = {
      if (count == 2) first + second
      else fibRecursive(second, first + second, count - 1)
    }
    n match {
      case 0 => 0
      case 1 => 1
      case _ => fibRecursive(0, 1, n)
    }
  }

  // Exercise p.29 --> verify if an Array is sorted according to an ordering function
  def isSorted[@specialized A](as: Array[A], gt: (A, A) => Boolean): Boolean =
    as.forall { i =>
      val iPos = as.indexOf(i);
      iPos == as.length - 1 ||
        gt(i, as(iPos + 1))
    }

  // Exercise p.32 --> implement partial1
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)
}