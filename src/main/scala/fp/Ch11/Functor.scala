package fp.Ch11

/** Functor abstraction
 *  representing a transformation, holding that
 *  map(v)(x => x) == v
 */
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  // Generic 'unzip' function
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map (fab)(_._1), map (fab)(_._2))
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B) = as map f
  }
}