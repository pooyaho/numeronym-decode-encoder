package ir.phsys.util

/**
 * @author : Пуя Гуссейни
 *         Email : info@pooya-hfp.ir
 *         Date: 1/20/14
 *         Time: 11:57 AM
 */
object MathUtil {

  /**
   *
   * @param n Size of series
   * @return  returns a probability series with less than e**5 error value
   */
  def generatePriorityProbabilitySeries(n: Int) = {
    val x = 2.0 / (n + 1)
    (1 to n).map(p => ((n - p + 1) / n.asInstanceOf[Float]) * x)
  }
}
