package mathematics

object NumUtils {
	implicit class IntUtils(n:Int){
	  def square = n * n
	  def abs = if(n < 0) -n else n
	}
}