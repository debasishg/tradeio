package tradex.domain
package http.routes

import java.time.LocalDate
import org.scalacheck.Gen
import cats.data.NonEmptyList
import cats.effect._
import cats.syntax.all._
import org.http4s.Method._
import org.http4s._
import org.http4s.client.dsl.io._
import org.http4s.syntax.literals._
import model.account._
import model.market._
import model.trade._
import repository.TradeRepository
import suite.HttpSuite
import model.TradeSuite
import cats.effect.unsafe.implicits.global

object TradeRoutesSuite extends HttpSuite {
  def dataTrades(trades: List[Trade]) = new TestTradeRepository {
    override def all: IO[List[Trade]]                                          = IO.pure(trades)
    override def query(accountNo: AccountNo, date: LocalDate): IO[List[Trade]] = IO.pure(trades)
    override def queryByMarket(market: Market): IO[List[Trade]]                = IO.pure(trades)
  }

  test("GET all trades succeeds") {
    forall(Gen.listOf(TradeSuite.tradeGen)) { trades =>
      val req = GET(uri"/trades")
      val ts  = trades.sequence.unsafeRunSync()
      val routes =
        TradeRoutes[IO](dataTrades(ts)).routes
      expectHttpBodyAndStatus(routes, req)(ts, Status.Ok)
    }
  }

  test("GET trades for Tokyo Market succeeds") {
    forall(Gen.listOf(TradeSuite.tradeForTokyoMarketGen)) { trades =>
      val req = GET(uri"/trades?market=Tokyo")
      val ts  = trades.sequence.unsafeRunSync()
      val routes =
        TradeRoutes[IO](dataTrades(ts)).routes
      expectHttpBodyAndStatus(routes, req)(ts, Status.Ok)
    }
  }
}

protected class TestTradeRepository extends TradeRepository[IO] {
  def query(accountNo: AccountNo, date: LocalDate): IO[List[Trade]] = IO.pure(List.empty[Trade])
  def queryByMarket(market: Market): IO[List[Trade]]                = IO.pure(List.empty[Trade])
  def all: IO[List[Trade]]                                          = IO.pure(List.empty[Trade])
  def store(trd: Trade): IO[Trade]                                  = ???
  def store(trades: NonEmptyList[Trade]): IO[Unit]                  = IO.pure(())
}
