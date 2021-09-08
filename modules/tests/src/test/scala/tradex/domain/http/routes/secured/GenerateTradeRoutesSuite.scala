package tradex.domain
package http.routes.secured

import java.time.LocalDate
import cats.effect._
import cats.data.{ Kleisli, NonEmptyList }
import model.account._
import model.market._
import model.trade._
import repository.TradeRepository
import suite.HttpSuite
import http.auth.users._
import org.http4s.server.AuthMiddleware

object GenerateTradeRoutesSuite extends HttpSuite {
  def authMiddleware(authUser: CommonUser): AuthMiddleware[IO, CommonUser] =
    AuthMiddleware(Kleisli.pure(authUser))

}

protected class TestTradeRepository extends TradeRepository[IO] {
  def query(accountNo: AccountNo, date: LocalDate): IO[List[Trade]] = IO.pure(List.empty[Trade])
  def queryByMarket(market: Market): IO[List[Trade]]                = IO.pure(List.empty[Trade])
  def all: IO[List[Trade]]                                          = IO.pure(List.empty[Trade])
  def store(trd: Trade): IO[Trade]                                  = ???
  def store(trades: NonEmptyList[Trade]): IO[Unit]                  = IO.pure(())
}
