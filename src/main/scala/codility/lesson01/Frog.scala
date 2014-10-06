package codility.lesson01

class Frog {
	/* This is the first problem in the codility lessons. There's a frog that needs to travel
	 * to the other side of the river by making jumps of size D. The frog is at position X and
	 * has to travel to position Y. Count the minimal number of jumps that the frog has to make*/
  
	def minimumJumps(x :Int,y :Int,d :Int) ={
	  if(((y.toFloat-x.toFloat)/d.toFloat).toString.split('.')(1) != "0") (y-x)/d + 1 else  (y-x)/d
	}
}