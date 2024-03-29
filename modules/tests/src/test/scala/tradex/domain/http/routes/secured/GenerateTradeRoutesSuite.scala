package tradex.domain
package http.routes.secured

import cats.effect._
import cats.data.{ Kleisli, NonEmptyList }

import io.circe.syntax._
import org.http4s.Method._
import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.client.dsl.io._
import org.http4s.server.AuthMiddleware
import org.http4s.syntax.literals._

import suite.HttpSuite
import http.auth.users._
import generators._

import model.instrument._
import model.trade._
import services.trading._
import services.accounting._
import repository.interpreter.memory._
import programs.GenerateTrade

object GenerateTradeRoutesSuite extends HttpSuite {
  def authMiddleware(authUser: CommonUser): AuthMiddleware[IO, CommonUser] =
    AuthMiddleware(Kleisli.pure(authUser))

  test("Generate trade succeeds") {
    val trading = for {
      a <- AccountRepositoryInterpreter.make[IO]
      e <- ExecutionRepositoryInterpreter.make[IO]
      o <- OrderRepositoryInterpreter.make[IO]
      t <- TradeRepositoryInterpreter.make[IO]
    } yield Trading.make[IO](a, e, o, t)

    val accounting = for {
      b <- BalanceRepositoryInterpreter.make[IO]
    } yield Accounting.make[IO](b)

    val genTrade = for {
      t <- trading
      a <- accounting
    } yield GenerateTrade(t, a)

    val gen = for {
      u <- commonUserGen
      t <- generateTradeFrontOfficeInputGen(frontOfficeOrderGen)
    } yield (u, t)

    forall(gen) { case (user, foTrades) =>
      val req = POST(foTrades, uri"/generatetrade")
      val routes = for {
        t <- trading
        a <- accounting
        g <- genTrade
      } yield GenerateTradeRoutes[IO](g, t, a).routes(authMiddleware(user))
      routes.flatMap(r => expectHttpStatus(r, req)(Status.Created))
    }
  }

  test("Generate trade fails") {
    val trading = for {
      a <- AccountRepositoryInterpreter.make[IO]
      e <- ExecutionRepositoryInterpreter.make[IO]
      o <- OrderRepositoryInterpreter.make[IO]
      t <- TradeRepositoryInterpreterWithFailedStore.make
    } yield Trading.make[IO](a, e, o, t)

    val accounting = for {
      b <- BalanceRepositoryInterpreter.make[IO]
    } yield Accounting.make[IO](b)

    val genTrade = for {
      t <- trading
      a <- accounting
    } yield GenerateTrade(t, a)

    val gen = for {
      u <- commonUserGen
      t <- generateTradeFrontOfficeInputGen(frontOfficeOrderGen)
    } yield (u, t)

    forall(gen) { case (user, foTrades) =>
      val req = POST(foTrades, uri"/generatetrade")
      val routes = for {
        t <- trading
        a <- accounting
        g <- genTrade
      } yield GenerateTradeRoutes[IO](g, t, a).routes(authMiddleware(user))

      val expected = "Failed in persistence of trades".asJson
      routes.flatMap(r => expectHttpBodyAndStatus(r, req)(expected, Status.BadRequest))
    }
  }
}

class TradeRepositoryInterpreterWithFailedStore(
    repo: Ref[IO, Map[(String, ISINCode, TradeReferenceNo), Trade]]
) extends TradeRepositoryInterpreter[IO](repo) {
  override def store(trades: NonEmptyList[Trade]): IO[Unit] =
    IO.raiseError(new Exception("Failed in persistence of trades"))
}

// Smart constructor
object TradeRepositoryInterpreterWithFailedStore {
  def make: IO[TradeRepositoryInterpreterWithFailedStore] =
    Ref
      .of[IO, Map[(String, ISINCode, TradeReferenceNo), Trade]](Map.empty)
      .map(new TradeRepositoryInterpreterWithFailedStore(_))
}
