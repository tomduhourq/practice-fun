package fp.Ch4

import java.util.regex.Pattern

/**
 * I am not using scala's None nor Some since they interfere in the implementations.
 * So Some is replaced by Algo and None is replaced by Nada.
 * Created by tomasduhourq on 2/19/15.
 */
sealed trait Option[+A] {
  // Exercise 1 p.55 --> implement map,flatMap,getOrElse,orElse,filter for Option
  def map[B](f: A => B): Option[B] = this match {
    case Nada => Nada
    case Algo(a) => Algo(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Nada => Nada
    case Algo(a) => f(a)
  }
  // B must be a supertype of A, and default is passed as a call-by-name parameter.
  def getOrElse[B >: A](default: => B): B = this match {
    case Nada => default
    case Algo(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Nada => ob
    case Algo(_) => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Algo(a) if f(a) => this
    case _ => Nada
  }
}

case class Algo[+A](get: A) extends Option[A]
case object Nada extends Option[Nothing]

// For the exercises which follow, I'll put them in the companion object
object Option {
  def mean(xs: Seq[Double]): Option[Double] = if(xs.isEmpty) Nada else Algo(xs.sum / xs.length)

  // NTH --> lift: transform an ordinary function from A => B to another of type Option[A] => Option[B]
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  // Exercise 2 p.56 --> variance: if m is the mean, then variance is the mean of math.pow(x - m, 2)
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // Exercise 3 p.59 --> map2: combines two Option values using a binary function.
  def map2[A,B,C](a: Option[A],b: Option[B])(f: (A,B) => C): Option[C] = a.flatMap(x => b.map(y => f(x,y)))

  // Exercise 5 p.59 --> sequence: combine a List[Option[A]] into one option containing a List of all Some in the list.
  def sequence[A](l: List[Option[A]]): Option[List[A]] = l match {
    case Nil => Algo(Nil)
    case x :: t => x.flatMap(y => sequence(t).map(y :: _))
  }

  // Exercise 6 p.60 --> traverse: maps a List[A] to an Option of the transformations of applying f.
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Algo(Nil)
      case x :: xs => map2(f(x), traverse(xs)(f))(_ :: _) // My f for map2 would be cons, so C is List[B]
    }

  def sequenceByTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)
}