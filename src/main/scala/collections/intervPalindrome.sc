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
  else Some(palindromes.reduce((max, next) =>
    if(max.length < next.length) next else max))
}


greatestPalindrome("aoboaccccc") == Some("aoboa")
greatestPalindrome("tomas") == None
greatestPalindrome("") == Some("")
greatestPalindrome("anitalavalatina") == Some("anitalavalatina")
