package tradex.domain
package model

import cats.effect.IO
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers
import model.order._
import model.trade._
import generators._

object TradeSuite extends SimpleIOSuite with Checkers {

  test("Trade Generation") {
    forall(tradeGen) {
      _.flatMap { trd =>
        expect.eql(BuySell.Buy, trd.buySell) or expect.eql(BuySell.Sell, trd.buySell)
        expect.eql(None, trd.valueDate)
        expect.eql(None, trd.netAmount)
        expect.eql(List.empty[TradeTaxFee], trd.taxFees)
        IO(success)
      }
    }
  }

  test("Trade with TaxFee and Net Amount Generation") {
    forall(tradeWithTaxFeeGen) {
      _.flatMap { trd =>
        expect.eql(BuySell.Buy, trd.buySell)
        expect.eql(None, trd.valueDate)
        expect(trd.netAmount.isEmpty).failFast
        expect(trd.taxFees.isEmpty).failFast
        IO(success)
      }
    }
  }
}
