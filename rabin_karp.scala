 /**
 * Substring search with the Rabin-Karp algorithm.
 * Returns the starting index of the first occurrence of the pattern
 * in the text, or -1 if the pattern does not occur in the text.
 */
def findSubstring(text: String, pattern: String): Int = {
  val n = text.size
  val m = pattern.size
  if(m > n)
    return -1
    
  val k = 101
  var pow = 1
  var pHash = 0
  var h = 0
  var i = 0
  var j = 0
  var p = 0
  // Compute initial hash and pattern hash
  i = m - 1
  while (i >= 0) {
    pHash += pow * pattern(i).toInt
    h += pow * text(i).toInt
    p = pow
    pow = pow * k
    i -= 1
  }
  
  i = 0
  // Rolling hash function
  while (i <= n - m) {  
    if (h == pHash && text.substring(i, i + m) == pattern) {
      return i
    }
    if (i == n - m) {
      return -1
    }
    h -= text(i).toInt * p
    h = h * k
    h += text(i + m)
    i += 1
  }
  return(-1)
}
