package tradex.domain
package config

import scala.concurrent.duration._
import com.comcast.ip4s.{Host, Port}

import eu.timepit.refined.types.net.UserPortNumber
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString

object config {
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
