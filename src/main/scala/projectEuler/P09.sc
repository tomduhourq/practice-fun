package projectEuler
import mathematics.NumUtils.IntUtils
object P09 {
  	/* A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

		a2 + b2 = c2
		For example, 32 + 42 = 9 + 16 = 25 = 52.

		There exists exactly one Pythagorean triplet for which a + b + c = 1000.
		Find the product abc. */
		(for{
		a <- (1 until 1000)
		b <- (a until 1000)
		c <- (b until 1000)
		if(a + b + c == 1000 && a.square + b.square == c.square)}
		yield a*b*c)
		.head                             //> res0: Int = 31875000
		// the Pythagorean triple is (200,375,425)
}