package tradex.domain

import cats.effect._
import cats.effect.unsafe.implicits.global

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import repository._
import trading._
import accounting._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    implicit val logger = Slf4jLogger.getLogger[IO]

    val trades =
      config.load[IO].flatMap { cfg =>
        Logger[IO].info(s"Loaded config $cfg") >>
          AppResources.make[IO](cfg).use { res =>
            Programs.make[IO]().flatMap { programs =>
              programs.generateTrade(
                Trading.make[IO](
                  AccountRepository.make[IO](res.psql),
                  ExecutionRepository.make[IO](res.psql),
                  OrderRepository.make[IO](res.psql),
                  TradeRepository.make[IO](res.psql)
                ),
                Accounting.make[IO](BalanceRepository.make[IO](res.psql))
              )
            }
          }
      }

    trades
      .flatMap { ts =>
        IO {
          val trades = ts._1
          val balance = ts._2
          trades.toList.foreach(println)
          balance.toList.foreach(println)
        }
      }
      .unsafeRunSync()

    IO(ExitCode.Success)
  }
}
