package tradex.domain
package model

import cats.effect.IO
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers
import model.order._
import model.trade._
import generators._

object TradeSuite extends SimpleIOSuite with Checkers {

  test("Trade Generation succeeds") {
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

  test("Trade with TaxFee and Net Amount Generation succeeds") {
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

  test("Trade Generation fails due to invalid trade date / value date combination") {
    forall(tradeWithInvalidTradeValueDateGen) {
      _.flatMap { _ =>
        IO(failure("Should fail with trade date / value date validation failure"))
      }.handleError(th => expect.all(th.getMessage().contains("cannot be earlier than")))
    }
  }
}
