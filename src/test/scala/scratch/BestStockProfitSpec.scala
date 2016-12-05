package scratch

import org.scalatest.{FeatureSpec, Matchers}

class BestStockProfitSpec extends FeatureSpec with Matchers {

  feature("Find the maximum profit between stock prices over time (can't short sell)") {

    scenario("Find the maximum profit over a range of increasing and decreasing stock prices") {
      val stockPrices = List(10, 7, 5, 8, 11, 9)

      StockPrices.getMaxProfit(stockPrices) shouldBe Option(6)
    }

    scenario("Stock price that always falls should return minimum loss") {
      val stockPrices = List(50, 40, 30, 25, 10)

      StockPrices.getMaxProfit(stockPrices) shouldBe Option(-5)
    }

    scenario("More than 1 stock price is required to return a profit") {
      StockPrices.getMaxProfit(List.empty) shouldBe None
      StockPrices.getMaxProfit(List(100)) shouldBe None
    }

    scenario("Give a few FP style solutions") {
      val stockPrices = List(10, 7, 5, 8, 11, 9)

      StockPrices.getMaxProfitFP(stockPrices, useMatch = false) shouldBe Option(6)
      StockPrices.getMaxProfitFP(stockPrices, useMatch = true) shouldBe Option(6)

      StockPrices.getMaxProfitFP(List.empty) shouldBe None
      StockPrices.getMaxProfitFP(List(100)) shouldBe None
    }
  }

}
