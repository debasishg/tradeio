package tradex.domain

import cats.effect._
import cats.effect.unsafe.implicits.global

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import dev.profunktor.redis4cats.log4cats._

import resources.AppResources
import repository._
import services.trading._
import services.accounting._
import services.Programs

object Main extends IOApp.Simple {
  override def run: IO[Unit] = {
    implicit val logger = Slf4jLogger.getLogger[IO]

    val trades =
      config.load[IO].flatMap { cfg => // load config
        Logger[IO].info(s"Loaded application configuration $cfg") >>
          AppResources
            .make[IO](cfg)
            .use { res => // make resources based on config
              Programs
                .make[IO]()
                .flatMap { // make the program that will give me the generators
                  _.generateTrade(
                    Trading.make[IO](
                      AccountRepository.make[IO](res.postgres),
                      ExecutionRepository.make[IO](res.postgres),
                      OrderRepository.make[IO](res.postgres),
                      TradeRepository.make[IO](res.postgres)
                    ),
                    Accounting
                      .make[IO](BalanceRepository.make[IO](res.postgres))
                  )
                }
            }
      }

    IO {
      trades
        .flatMap { ts =>
          IO {
            val trades  = ts._1
            val balance = ts._2
            trades.toList.foreach(println)
            balance.toList.foreach(println)
          }
        }
        .unsafeRunSync()
    }
  }
}
