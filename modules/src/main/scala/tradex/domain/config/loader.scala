package tradex.domain
package config

import scala.concurrent.duration._
import cats.effect.Async
import cats.syntax.all._
import ciris._
import ciris.refined._
import environments._
import environments.AppEnvironment._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import com.comcast.ip4s._
import config._

object load {
  def apply[F[_]: Async]: F[AppConfig] =
    env("TRADING_APP_ENV")
      .as[AppEnvironment]
      .default(Prod)
      .flatMap {
        case Test => default
        case Prod => default
      }
      .load[F]

  private def default[F[_]]: ConfigValue[F, AppConfig] =
    (
      env("DATABASE_USER").as[NonEmptyString].default("postgres"),
      env("DATABASE_NAME").as[NonEmptyString].default("trading")
    ).parMapN { (dbuser, dbname) =>
      AppConfig(
        PostgreSQLConfig(
          host = "localhost",
          port = 5432,
          user = dbuser,
          database = dbname,
          max = 10
        ),
        HttpServerConfig(
          host = host"0.0.0.0",
          port = port"8080"
        ),
        HttpClientConfig(
          timeout = 60.seconds,
          idleTimeInPool = 30.seconds
        )
      )
    }
}
