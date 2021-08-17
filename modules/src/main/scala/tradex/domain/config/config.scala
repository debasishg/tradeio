package tradex.domain
package config

import ext.ciris._

import ciris.refined._

import scala.concurrent.duration._
import com.comcast.ip4s.{Host, Port}

import derevo.cats.show
import derevo.derive
import io.estatico.newtype.macros.newtype

import eu.timepit.refined.cats._
import eu.timepit.refined.types.net.UserPortNumber
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString

object config {
  @derive(configDecoder, show)
  @newtype
  case class PasswordSalt(secret: NonEmptyString)

  @newtype case class TokenExpiration(value: FiniteDuration)

  @derive(configDecoder, show)
  @newtype
  case class JwtAccessTokenKeyConfig(secret: NonEmptyString)

  case class AppConfig(
      postgreSQL: PostgreSQLConfig,
      httpServerConfig: HttpServerConfig,
      httpClientConfig: HttpClientConfig
  )

  case class PostgreSQLConfig(
      host: NonEmptyString,
      port: UserPortNumber,
      user: NonEmptyString,
      database: NonEmptyString,
      max: PosInt
  )

  case class HttpServerConfig(
      host: Host,
      port: Port
  )

  case class HttpClientConfig(
      timeout: FiniteDuration,
      idleTimeInPool: FiniteDuration
  )
}
