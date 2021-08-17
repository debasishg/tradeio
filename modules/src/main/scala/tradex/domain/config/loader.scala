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
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import com.comcast.ip4s._
import config._

object load {
  def apply[F[_]: Async]: F[AppConfig] =
    env("TRADING_APP_ENV")
      .as[AppEnvironment]
      .default(Prod)
      .flatMap {
        case Test => default(RedisURI("redis://localhost"))
        case Prod => default(RedisURI("redis://10.123.154.176"))
      }
      .load[F]

  private def default[F[_]](redisUri: RedisURI): ConfigValue[F, AppConfig] =
    (
      env("SC_JWT_SECRET_KEY").as[JwtSecretKeyConfig].secret,
      env("SC_JWT_CLAIM").as[JwtClaimConfig].secret,
      env("SC_ACCESS_TOKEN_SECRET_KEY").as[JwtAccessTokenKeyConfig].secret,
      env("SC_ADMIN_USER_TOKEN").as[AdminUserTokenConfig].secret,
      env("SC_PASSWORD_SALT").as[PasswordSalt].secret,
      env("SC_POSTGRES_PASSWORD").as[NonEmptyString].secret,
      env("DATABASE_USER").as[NonEmptyString].default("postgres"),
      env("DATABASE_NAME").as[NonEmptyString].default("trading")
    ).parMapN {
      (
          jwtSecretKey,
          jwtClaim,
          tokenKey,
          adminToken,
          salt,
          postgresPassword,
          dbuser,
          dbname
      ) =>
        AppConfig(
          AdminJwtConfig(jwtSecretKey, jwtClaim, adminToken),
          tokenKey,
          salt,
          TokenExpiration(30.minutes),
          PostgreSQLConfig(
            host = "localhost",
            port = 5432,
            user = dbuser,
            password = postgresPassword,
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
          ),
          RedisConfig(redisUri)
        )
    }
}
