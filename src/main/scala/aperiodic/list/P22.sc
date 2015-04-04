// Create a list containing all integers between given range
def range(lo: Int, hi: Int) = Range(4, 9).inclusive.toList // don't reinvent the wheel
def range2(lo: Int, hi: Int) = Stream.from(lo).take(hi - lo + 1).toList // because I can

