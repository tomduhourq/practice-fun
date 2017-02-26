package aperiodic

class P25[A] extends P23[A] {
  def randomPermute(list: List[A]): List[A] = randomSelect(list.length, list)
}

object P25 extends App {
  val p25 = new P25[Int]
  (1 to 10).foreach(_ => println(p25.randomPermute(List(1,2,3))))
}
