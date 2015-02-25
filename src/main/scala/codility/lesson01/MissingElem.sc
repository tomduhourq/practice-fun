package codility.lesson01

/**
 * Correctness: 100%
 * Performance: 60%
 * Created by tomasduhourq on 2/25/15.
 */
object MissingElem {

	def missingElem(A: Array[Int]):Int={
		val l = A.length + 1
		l*(l+1)/2 - A.sum
	}                                         //> missingElem: (A: Array[Int])Int
	
	missingElem(Array(4,1,5,3))               //> res0: Int = 2
}