package fp.Ch4

/* Represent values that can be of one of two types.
  By convention, the Left constructor is reserved for failures.*/
sealed trait Either[+E, +A] {
  // Exercise 7 p.61: implement map, flatMap, orElse, map2
  def map[B](f: A => B): Either[E,B] = this match {
    case Left(e) => Left(e)
    case Right(b) => Right(f(b))
  }

  def map2[EE >: E, B, C](b: Either[EE,B])(f: (A,B) => C): Either[EE, C] = (this, b) match {
    case (Left(a),_) => Left(a)
    case (_, Left(b)) => Left(b)
    case (Right(a), Right(b)) => Right(f(a,b))
  }

  // Exclusion of scala's Either was not working, so I typed my implementations.
  type Either[E,B] = fp.Ch4.Either[E,B]
  type Left[E] = fp.Ch4.Left[E]
  type Right[B] = fp.Ch4.Right[B]
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]