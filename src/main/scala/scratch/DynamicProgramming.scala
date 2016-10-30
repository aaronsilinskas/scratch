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

}
