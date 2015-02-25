package fp.Ch4

/* Represent values that can be of one of two types.
  By convention, the Left constructor is reserved for failures.*/
sealed trait Either[+E, +A] {

  // Exercise 7 p.61: implement map, flatMap, orElse, map2
  def map[B](f: A => B): Either[E,B] = this match {
    case Left(e) => Left(e)
    case Right(b) => Right(f(b))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(a) => Left(a)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(_) => this
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

object Either{

  // Exercise 8 --> sequence + traverse for Either (very similar to Option's)
  def traverse[E,A,B](l: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    l match {
      case Nil => Right(Nil)
      case h::t => (f(h).map2(traverse(t)(f)))(_ :: _)
    }
  
  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(identity)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]