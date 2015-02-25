package codility.lesson01

/**
 * Correctness: 66%
 * Performance: 100%
 */
object ArraySplitMinimum {

  def abs(x: Int): Int = if(x >= 0) x else -x

  def splitMinimum(a: Array[Int]):Int = {
  	var min = 9999999
  	var sumLeft = 0
  	var diff =0
  	var sumRight = a.sum
  	for(i <-0 until a.length){
  		sumLeft += a(i)
  		sumRight -= sumLeft
  		diff = abs(sumLeft - sumRight)
  		if(diff <= min)
  			min = diff
  		sumRight +=sumLeft
  	}
  	min
  }                                               //> splitMinimum: (a: Array[Int])Int
  
  splitMinimum(Array(3,1,2,4,3))                  //> res0: Int = 1
}