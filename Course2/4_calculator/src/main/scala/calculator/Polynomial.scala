package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
    val aVal = a()
    val bVal = b()
    val cVal = c()
    bVal * bVal - 4 * aVal * cVal
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    val deltaVal = delta()
    if (deltaVal < 0d) Set.empty
    else {
      val daVal = 2 * a()
      val mbVal = -b()
      val deltaVal = delta()
      if (deltaVal < 0) Set.empty
      else Set(
        (mbVal - math.sqrt(deltaVal))/daVal,
        (mbVal + math.sqrt(deltaVal))/daVal
      )
    }

  }
}
