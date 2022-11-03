/**
 * A parallel least-significant-digit first radix sort for integer arrays.
 * Sorts the argument array a in increasing order.
 */
def sortPar(a: Array[Int]): Unit = {
  val N = a.length
  if(N <= 1) return
  val P: Int = par.getParallelism
  val count = Array.tabulate(P,256)((i,j) => 0)  // Initialize counting array    
  var i,p = 0
  var j = 1
  val starts = (0 to P).map(i => (N * i) / P)
  val intervals = (starts zip starts.tail)
  
  while (p < 4) {
    i = 0
    j = 0
    if (p > 0) {
      // Reset count array
      while (i < P) {
        val countRow = count(i)
        j = 0
        while (j < countRow.length) {
          countRow(j) = 0
          j += 1
        }
        i += 1
      }
    }
    
    val countTasks = intervals.zipWithIndex.map ({
      case ((start, end), idx) => par.task {
        var i = start
        val row = count(idx)
        while (i < end) {
          row((a(i) >> p*8) & 0xff) += 1
          i += 1
        }
      }
    })
    
    countTasks.foreach(_.join())
   
    // Construct first index and - 1 because indexes start from 0
    var r = 1
    var c = 1
    count(0)(0) -= 1
    while (r < P) {
      count(r)(0) += count(r-1)(0)
      r += 1
    }
    // Transform count table into cumulative index table
    while (c < 256) {
      count(0)(c) = count(P - 1)(c - 1) + count(0)(c)
      r = 1
      while (r < P) {
        count(r)(c) = count(r-1)(c) + count(r)(c)
        r += 1
      }
      c += 1
    }
    
    // Reconstruct intermediate result
    val aux = a.clone //new Array[Int](N)
    val resultTask = intervals.zipWithIndex.map({
      case ((start, end), idx) => par.task {
        var i = end - 1
        var row = count(idx)
        while (i >= start) {
          a(row((aux(i) >> p*8) & 0xff)) = aux(i)
          row((aux(i) >> p*8) & 0xff) -=  1
          i -= 1
        }
      }
    })
    resultTask.foreach(_.join())
    p += 1
  }
}

