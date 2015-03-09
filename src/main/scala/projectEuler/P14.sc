/**
 * Couldn't import my own mathematics utils module,
 * so definition is duplicated
 */

object Utils {
  implicit class IntUtils(n: Long){
    def collatzSequence: List[Long] = {
      def recCollatz(c: Long): List[Long] = {
        if (c == 1) 1 :: Nil
        else {
          if (c % 2 == 0) (c / 2) :: recCollatz(c / 2)
          else (3 * c + 1) :: recCollatz(3 * c + 1)
        }
      }
      recCollatz(n)
    }
  }
}

import Utils.IntUtils

lazy val collatzSizes : Stream[(Long,Long)] =
  Stream.from(1).map(n => (n.toLong, n.collatzSequence.size.toLong))

collatzSizes.takeWhile(1000000>_._1).maxBy(_._2)._1 // 837799

