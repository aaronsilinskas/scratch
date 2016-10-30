package scratch

object DynamicProgramming {

  def minimumStepsTo1(value: Int): Int = {
    val memo: Array[Int] = Array.ofDim(value + 1)

    def iter(value: Int): Int = {
      if (value == 1) {
        0
      } else {
        if (memo(value) == 0) {
          memo(value) = 1 + iter(value - 1)
          if (value % 2 == 0) {
            memo(value) = Math.min(memo(value), 1 + iter(value / 2))
          }
          if (value % 3 == 0) {
            memo(value) = Math.min(memo(value), 1 + iter(value / 3))
          }
        }
        memo(value)
      }
    }

    iter(value)
  }
  
}
