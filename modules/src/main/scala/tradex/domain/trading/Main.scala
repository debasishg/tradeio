package tradex.domain
package trading

import cats.data._
import cats.effect._
import cats.implicits._

import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

import model.market._

import repository._

import Implicits._
import AppData._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val csvOrder = orderGenerator.generateOrders()
    val brokerAccountNo = ano3
    val clientAccounts = NonEmptyList.of(ano1, ano2)

    implicit val logger = Slf4jLogger.getLogger[IO]

    def askRepo[A](repo: A) = DefaultApplicativeAsk.constant[IO, A](repo)

    val trades =
      config.load[IO].flatMap { cfg =>
        Logger[IO].info(s"Loaded config $cfg") >>
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
            implicit val balanceRepositoryAsk =
              askRepo[BalanceRepository[IO]](algebras.balanceRepository)

            program.tradeGeneration(
              new TradingInterpreter[IO],
              new AccountingInterpreter[IO],
              csvOrder,
              brokerAccountNo,
              Market.NewYork,
              clientAccounts
            )
          }
        }
      }
    trades.map(_._1).unsafeRunSync().toList.foreach(println)
    trades.map(_._2).unsafeRunSync().toList.foreach(println)
    IO(ExitCode.Success)
  }
}
