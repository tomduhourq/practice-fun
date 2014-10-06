package codility.lesson01

object ArraySplitMinimum {

	def sum(a:Array[Int]):Int ={
		var sum = 0
		for(i<-0 until a.length)
			sum += a(i)
			sum
	}                                         //> sum: (a: Array[Int])Int
	
  def abs(x: Int): Int = if(x>=0) x else -x       //> abs: (x: Int)Int
  //--Got 100% performance and 66% correctness
  def splitMinimum(a: Array[Int]):Int = {
  	var min = 9999999
  	var sumLeft = 0
  	var diff =0
  	var sumRight = sum(a)
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