package scratch

import scala.annotation.tailrec

object StockPrices {

  def getMaxProfit(stockPrices: List[Int]): Option[Int] = {
    if (stockPrices.length < 2) {
      None
    } else {
      var maxProfit = stockPrices(1) - stockPrices(0)
      var minimumPrice = Math.min(stockPrices(0), stockPrices(1))

      for (i <- 2 until stockPrices.size) {
        val currentPrice = stockPrices(i)
        val currentProfit = currentPrice - minimumPrice

        maxProfit = Math.max(currentProfit, maxProfit)

        if (currentPrice < minimumPrice) {
          minimumPrice = currentPrice
        }
      }

      Option(maxProfit)
    }
  }

  def getMaxProfitFP(stockPrices: List[Int], useMatch: Boolean = false): Option[Int] = {
    if (stockPrices.length < 2) {
      None
    } else {
      val maxProfit = stockPrices(1) - stockPrices(0)
      val minimumPrice = Math.min(stockPrices(0), stockPrices(1))

      val remainingStockPrices = stockPrices.drop(2)

      if (useMatch) {
        Option(getMaxProfitWithMatch(remainingStockPrices, minimumPrice, maxProfit))
      } else {
        Option(getMaxProfit(remainingStockPrices, minimumPrice, maxProfit))
      }
    }
  }

  @tailrec
  def getMaxProfit(stockPrices: List[Int], minimumPrice: Int, maxProfit: Int): Int = {
    if (stockPrices.isEmpty) {
      maxProfit
    } else {
      val currentPrice = stockPrices.head
      val currentProfit = currentPrice - minimumPrice

      val updatedMinimumPrice = Math.min(currentPrice, minimumPrice)
      val updatedMaxProfit = Math.max(currentProfit, maxProfit)

      getMaxProfit(stockPrices.tail, updatedMinimumPrice, updatedMaxProfit)
    }
  }

  @tailrec
  def getMaxProfitWithMatch(stockPrices: List[Int], minimumPrice: Int, maxProfit: Int): Int = {
    stockPrices match {
      case head :: tail =>
        val currentPrice = stockPrices.head
        val currentProfit = currentPrice - minimumPrice

        val updatedMinimumPrice = Math.min(currentPrice, minimumPrice)
        val updatedMaxProfit = Math.max(currentProfit, maxProfit)

        getMaxProfitWithMatch(stockPrices.tail, updatedMinimumPrice, updatedMaxProfit)
      case _ =>
        maxProfit
    }
  }

}
