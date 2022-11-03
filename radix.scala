
/**
* Least-significant-digit-first radix sort for positive integer arrays.
* Sorts the argument array in ascending order.
*/
def lsdRadixSort(a: Array[Int]): Unit = {
  val N = a.length
  if(N <= 1) return
  
  val count = Array.fill(256)(0)   // Initialize counting array    
  var i,p = 0
  var j = 1
  
  while (p < 4) {    
    if (p > 0) {
      // Reset counting array
      while (i < count.length) {
        count(i) = 0
        i += 1
      }
    }
    
    // Initialize intermediate array
    val bb = a.clone
    i = 0
    
    // Count number of same byte
    while (i < a.length) {
      count((bb(i) >> p*8) & 0xff) += 1
      i += 1
    }
    
    // Convert count array to index array
    count(0) -= 1
    i = 1
    while (i < count.length) {
      count(i) = count(i-1) + count(i)
      i += 1
    }
    
    // Use index array to put values back into a in sorted order 
    i = a.length - 1
      while (i >= 0) {
        a(count((bb(i) >> p*8) & 0xff)) = bb(i)
        count((bb(i) >> p*8) & 0xff) -=  1
        i -= 1
      }
    
    i = 0
    p += 1
  }  
}