package tradex.domain

import modules._
import resources._

import cats.effect._
import cats.effect.std.Supervisor
import org.typelevel.log4cats.slf4j.Slf4jLogger
import dev.profunktor.redis4cats.log4cats._

import eu.timepit.refined.auto._
import org.typelevel.log4cats.Logger

object MainS extends IOApp.Simple {
  implicit val logger = Slf4jLogger.getLogger[IO]

  override def run: IO[Unit] =
    config.load[IO].flatMap { cfg =>
      Logger[IO].info(s"Loaded config $cfg") >>
        Supervisor[IO].use { implicit sp =>
          AppResources
            .make[IO](cfg)
            .evalMap { res =>
              Security
                .make[IO](cfg, res.postgres, res.redis)
                .map { security =>
                  val services =
                    Services.make[IO](res.postgres, res.redis)
                  val programs =
                    Programs.make[IO](services)
                  val api = HttpApi.make[IO](services, programs, security)
                  cfg.httpServerConfig -> api.httpApp
                }
            }
            .flatMap {
              case (cfg, httpApp) =>
                MkHttpServer[IO].newEmber(cfg, httpApp)
            }
            .useForever
        }
    }
}
