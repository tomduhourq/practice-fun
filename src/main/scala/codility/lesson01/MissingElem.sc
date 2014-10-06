package codility.lesson01

object MissingElem {
	
	/* For this one we need to get the missing number inside an array which follows the
		 pattern [1..N+1] -> Got 100% correctness and 60% performance*/
 def sum(a:Array[Int]):Int ={
		var sum = 0
		for(i<-0 until a.length)
			sum += a(i)
		sum
	}                                         //> sum: (a: Array[Int])Int
	
	def missingElem(A: Array[Int]):Int={
		val l = A.length + 1
		l*(l+1)/2 - sum(A)
	}                                         //> missingElem: (A: Array[Int])Int
	
	missingElem(Array(4,1,5,3))               //> res0: Int = 2
}