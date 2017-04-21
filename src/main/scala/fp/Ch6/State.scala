package fp.Ch6

/**
  * Created by tduhourq on 19/4/17.
  */
case class State[S, +A](run: S => (A, S)) {
  import State._
  // State(s => { val (a, s1) = run(s); (f(a), s1)})
  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State( s => {
    val (a, s1) = run(s)
    val (b, s2) = sb.run(s1)
    (f(a, b), s2)
  }
  )
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  }
  )
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State(s => {
    fs.foldLeft(unit(List.empty[A]).run(s)){ case ((partialList, acumState), state) =>
      val (elem, nextState) = state.run(acumState)
      (partialList :+ elem, nextState)
    }
  })

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def update(input: Input): Machine = (input, this) match {
      case (_, m @ Machine(_, 0, _)) => m
      case (_, m @ Machine(true, _, _)) => m
      case (Turn, Machine(false, _, coins)) => Machine(locked = true, 0, coins)
      case (Coin, m @ Machine(false, candies, coins)) if candies > 0 => m.copy(locked = true, coins = coins + 1)
      case other => other._2
    }
  }
}

