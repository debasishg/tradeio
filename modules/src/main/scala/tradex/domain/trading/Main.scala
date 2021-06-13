package tradex.domain
package trading

import cats.effect._
import cats.implicits._
import cats.effect.unsafe.implicits.global

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import repository._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    implicit val logger = Slf4jLogger.getLogger[IO]

    val trades =
      config.load[IO].flatMap { cfg =>
        Logger[IO].info(s"Loaded config $cfg") >>
          AppResources.make[IO](cfg).use { res =>
            Algebras.make[IO](res.psql).flatMap { algebras =>
              Programs.make[IO](algebras).flatMap { programs =>
                implicit val accountRepositoryAsk =
                  askRepo[AccountRepository[IO]](algebras.accountRepository)
                implicit val executionRepositoryAsk =
                  askRepo[ExecutionRepository[IO]](algebras.executionRepository)
                implicit val orderRepositoryAsk =
                  askRepo[OrderRepository[IO]](algebras.orderRepository)
                implicit val tradeRepositoryAsk =
                  askRepo[TradeRepository[IO]](algebras.tradeRepository)
                implicit val balanceRepositoryAsk =
                  askRepo[BalanceRepository[IO]](algebras.balanceRepository)

                programs.generateTrade(
                  new TradingInterpreter[IO],
                  new AccountingInterpreter[IO]
                )
              }
            }
          }
      }

    // FIXME
    trades.map(_._1).unsafeRunSync() // .toList.foreach(println)
    trades.map(_._2).unsafeRunSync() // .toList.foreach(println)
    IO(ExitCode.Success)
  }
}
