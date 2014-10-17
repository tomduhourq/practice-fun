package aperiodic

object P06palindrome {
 /* In this task we need to know whether a List is a palindrome or not */
 def palindrome(l :List[Any]):Boolean =
 	l match {
 		//--Putting ´empty´ here would yield false
 		case List() => true
 		case x :: Nil => true
 		case x :: xs => x == xs.last && palindrome(xs dropRight 1)
 }                                                //> palindrome: (l: List[Any])Boolean
 palindrome(List("pera","caca","pera"))           //> res0: Boolean = true
 palindrome(List(1,2,3,1,3,2,1))                  //> res1: Boolean = true
 palindrome(List("a","b"))                        //> res2: Boolean = false
}