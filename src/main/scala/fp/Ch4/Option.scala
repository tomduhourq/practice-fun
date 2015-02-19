package fp.Ch4

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