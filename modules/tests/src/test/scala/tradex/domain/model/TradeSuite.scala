package tradex.domain
package model

import java.time.LocalDateTime
import cats.effect.IO
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers
import org.scalacheck.Gen
import model.order._
import model.market._
import model.trade._
import generators._

object TradeSuite extends SimpleIOSuite with Checkers {
  val tradeGen = for {
    no   <- accountNoGen
    isin <- isinGen
    mkt  <- Gen.oneOf(Market.NewYork, Market.Tokyo, Market.HongKong)
    bs   <- Gen.const(BuySell.Buy)
    up   <- unitPriceGen
    qty  <- quantityGen
    td   <- Gen.oneOf(List(LocalDateTime.now, LocalDateTime.now.plusDays(2)))
    vd   <- Gen.const(None)
  } yield Trade.trade[IO](no, isin, mkt, bs, up, qty, td, vd)

  test("Trade Generation") {
    forall(tradeGen) {
      _.flatMap { trd =>
        expect.same(BuySell.Buy, trd.buySell)
        expect.eql(None, trd.valueDate)
        expect.eql(None, trd.netAmount)
        expect.eql(List.empty[TradeTaxFee], trd.taxFees)
        IO(success)
      }
    }
  }
}
