/** As we are only going to move right or down
  * for the 2x2 grid we have 4! of arranging RRDD,
  * but R1R2 and R2R1 doesn't count because we
  * only go one way, so we are discarding 2! from the
  * right movements and 2! from the down movements.
  * For this problem we have 20 rights and 20 downs.
  * So the rule is a!/b!(a - b)! (definition of combinatorial number)
  * with a = m + n = 20 + 20 = 40 in our case
  * and b = 20
  */
object NumUtils {
  implicit class IntUtils(val n:Int){
    def ! = {
      def recFactorial(acum: BigInt, left: Int): BigInt =
        if(left == 1) acum
        else recFactorial(acum * left, left - 1)
      recFactorial(BigInt(1),n)
    }
  }
}
import NumUtils._
(40!)/((20!)*(20!)) // 137846528820
