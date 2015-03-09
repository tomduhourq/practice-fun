package mathematics

object NumUtils {
	implicit class IntUtils(n:Int){
	  def square = n * n
	  def abs = if(n < 0) -n else n
		def collatzSequence: List[Int] = {
			def recCollatz(c: Int): List[Int] = {
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