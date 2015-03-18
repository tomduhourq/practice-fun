package fp.Ch10

sealed trait WC
// Abstraction representing a string that is not complete
case class Stub(chars: String) extends WC
// Abstraction that keeps the number of words seen so far
// lStub holds any partial word to the left and rStub the ones on the right
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(a), Stub(b))                   => Stub(a + b)
      case (Stub(a), Part(l, w, r))             => Part(a + l, w, r)
      case (Part(l, w, r), Stub(b))             => Part(l, w, r + b)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + determineComplete(r1, l2) + w2, r2)
    }
    override val zero: WC = Stub("")

    private def determineComplete(r1: String, l2: String) =
      if((r1 + l2).isEmpty) 0
      else 1
  }
}