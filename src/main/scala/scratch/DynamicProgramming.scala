package scratch

object DynamicProgramming {

  def minimumStepsTo1(value: Int): Int = {
    val memo: Array[Int] = Array.ofDim(value + 1)
    memo(0) = 0
    memo(1) = 0
    for (n <- 2 to value) {
      memo(n) = 1 + memo(n - 1)
      if (n % 2 == 0) {
        memo(n) = Math.min(memo(n), 1 + memo(n / 2))
      }
      if (n % 3 == 0) {
        memo(n) = Math.min(memo(n), 1 + memo(n / 3))
      }
    }

    memo(value)
  }

  def longestSubsequence(value: Array[Int]) = {
    val memo: Array[Int] = Array.ofDim(value.length)
    memo(0) = 1
    for (i <- 1 until value.length) {
      memo(i) = 1
      
      for (j <- (i - 1) to 0 by -1) {
        if (value(i) > value(j)) {
          memo(i) = Math.max(memo(i), memo(j) + 1)
        }
      }
    }

    memo.max
  }

}
