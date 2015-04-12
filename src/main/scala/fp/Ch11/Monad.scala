package fp.Ch11

import fp.Ch4.{Algo, Option}

/** FlatMappable abstraction.
 * All monads are functors, but not all functors
 * are monads.
 */
sealed trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))
}

// Exercise 1 p.194 --> Write monad instances for Par, Parser, Option, Stream, and List
object Monad {
  val optionMonad = new Monad[Option]{
    def unit[A](a: => A) = Algo(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
  }
}