package aperiodic
object P09pack {
  def pack[A](l: List[A]): List[List[A]] = {
    if (l.isEmpty) List(Nil)
    else {
      val (packing, next) = l.span(l.head ==)
      if (next == Nil) List(packing)
      else packing :: pack(next)
    }
  }
}