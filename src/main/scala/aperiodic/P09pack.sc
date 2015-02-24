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

  // P10 aperiodic
  def encode(l: List[Any]): List[Any] = pack(l).map{list => (list.length,list.head)}

  // P11 aperiodic
  def encodeModified(l: List[Any]): List[Any] =
    pack(l)
      .map{
      case l if l.length == 1 => l.head
      case l if l.length > 1 => (l.length,l.head)
    }
}