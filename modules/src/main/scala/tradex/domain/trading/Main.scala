package tradex.domain
package trading

import cats._
import cats.data._
import cats.implicits._
import cats.effect._

import model.newtypes._
import model.market._

import repository._

import Implicits._
import AppData._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val csvOrder = orderGenerator.generateOrders()
    val brokerAccountNo = ano3
    val clientAccounts = NonEmptyList.of(ano1, ano2)

    def askRepo[A](repo: A) = DefaultApplicativeAsk.constant[IO, A](repo)

    val trades =
      config.load[IO].flatMap { cfg =>
        AppResources.make[IO](cfg).use { res =>
          Algebras.make[IO](res.psql).flatMap { algebras =>
            implicit val accountRepositoryAsk =
              askRepo[AccountRepository[IO]](algebras.accountRepository)
            implicit val executionRepositoryAsk =
              askRepo[ExecutionRepository[IO]](algebras.executionRepository)
            implicit val instrumentRepositoryAsk =
              askRepo[InstrumentRepository[IO]](algebras.instrumentRepository)
            implicit val orderRepositoryAsk =
              askRepo[OrderRepository[IO]](algebras.orderRepository)
            implicit val tradeRepositoryAsk =
              askRepo[TradeRepository[IO]](algebras.tradeRepository)

            program.tradeGeneration(
              new TradingInterpreter[IO],
              csvOrder,
              brokerAccountNo,
              Market.NewYork,
              clientAccounts
            )
          }
        }
      }
    trades.unsafeRunSync.toList.foreach(println)
    IO(ExitCode.Success)
  }
}
