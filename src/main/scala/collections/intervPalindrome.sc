//Check if string is palindrome
def palindrome(str: String) = str.reverse == str
palindrome("anitalavalatina")
def palindrome2(str: String): Boolean = {
  val size = str.length
  @annotation.tailrec
  def palRec(n: Int): Boolean =
    if(n == size/2) true
    else {
      if (str.charAt(n) != str.charAt(size - 1 - n)) false
      else true && palRec(n + 1)
    }
  palRec(0)
}
palindrome2("anitalavalatina")
palindrome2("caoboac")
def obtainSubstrings(i: Int, str: String, size: Int): List[String] = {
  (str.substring(0, i) :: (i until size).view.map(n => str.substring(i, n + 1)).toList)
    .filter(s => s.length > 1 && palindrome(s))
}
// Find the greatest palindrome in a given string
def greatestPalindrome(str: String): Option[String] =
if(palindrome(str)) Some(str)
else {
  val size = str.length
  val palindromes =
    for {
      i <- 1 until size
      palindrome <- obtainSubstrings(i, str, size)
    } yield palindrome
  if(palindromes.isEmpty) None
  else //Some(palindromes.reduce((max, next) =>
    //if(max.length < next.length) next else max))
    Some(palindromes.sortBy(_.length).last)
}
greatestPalindrome("aoboacccc") == Some("aoboa")
greatestPalindrome("tomas") == None
greatestPalindrome("") == Some("")
greatestPalindrome("anitalavalatina") == Some("anitalavalatina")


// Given a string and a number k, find the string with the
// largest k different chars

def greatestDiffChars(str: String, k: Int): Option[String] = {
  // 1 - find substrings >= k in length
  // 2 - filter diff chars
  // 3 - max
  def containsKDifferentChars(s: String) = s.groupBy(identity).keySet.size == k

  def getDifferentSubstrings(size: Int, i: Int): IndexedSeq[String] = {
    (i until size)
      .map(pos => str.substring(i, pos + 1))
      .filter(s =>
        s.length >= k &&
        containsKDifferentChars(s))
  }

  if(str.isEmpty) None
  else {
    val size = str length
    val substringsGreaterThanK =
      for {
        i <- str.indices
        x <- getDifferentSubstrings(size, i)
      } yield x
    if(substringsGreaterThanK.isEmpty) None
    else Some(
      //substringsGreaterThanK.reduce((a, b) => if(a.length > b.length) a else b)
      substringsGreaterThanK.sortBy(_.length).last
    )
  }
}

greatestDiffChars("acboabs", 4)

