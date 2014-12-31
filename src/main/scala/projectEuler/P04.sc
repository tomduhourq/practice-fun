package projectEuler

object P04 {
  /*A palindromic number reads the same both ways.
  The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
	Find the largest palindrome made from the product of two 3-digit numbers*/
	(100 to 999)
	// We need flatMap here because only mapping will give us a Vector of Vectors for each number up to 999
	.flatMap(i => (i to 999).map(i*))
	.filter(p => p.toString == p.toString.reverse)
	.max                                      //> res0: Int = 906609
}