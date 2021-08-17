package tradex.domain
package modules

import org.typelevel.log4cats.Logger
import scala.concurrent.duration._
import cats.effect.Async
import cats.syntax.all._
import http.routes._
import http.routes.secured._
import org.http4s._
import org.http4s.implicits._
import org.http4s.server.middleware._
import dev.profunktor.auth.JwtAuthMiddleware
import http.auth.users.CommonUser

object HttpApi {
  def make[F[+_]: Async: Logger](
      services: Services[F],
      programs: Programs[F],
      security: Security[F]
  ): HttpApi[F] =
    new HttpApi[F](services, programs, security) {}
}

sealed abstract class HttpApi[F[+_]: Async: Logger] private (
    services: Services[F],
    programs: Programs[F],
    security: Security[F]
) {
  private val usersMiddleware =
    JwtAuthMiddleware[F, CommonUser](
      security.userJwtAuth.value,
      security.usersAuth.findUser
    )

  private val accountRoutes =
    AccountRoutes[F](services.accountRepository).routes
  private val balanceRoutes =
    BalanceRoutes[F](services.balanceRepository).routes
  private val tradeRoutes =
    TradeRoutes[F](services.tradeRepository).routes
  private val healthRoutes =
    HealthRoutes[F](services.healthCheck).routes
  private val generateTradeRoutes =
    GenerateTradeRoutes[F](
      programs.generateTrade,
      services.trading,
      services.accounting
    ).routes(usersMiddleware)

  private val openRoutes: HttpRoutes[F] =
    accountRoutes <+> balanceRoutes <+> tradeRoutes <+> healthRoutes <+> generateTradeRoutes

  private val middleware: HttpRoutes[F] => HttpRoutes[F] = {
    { http: HttpRoutes[F] =>
      AutoSlash(http)
    } andThen { http: HttpRoutes[F] =>
      CORS(http)
    } andThen { http: HttpRoutes[F] =>
      Timeout(60.seconds)(http)
    }
  }

  private val loggers: HttpApp[F] => HttpApp[F] = {
    { http: HttpApp[F] =>
      RequestLogger.httpApp(true, true)(http)
    } andThen { http: HttpApp[F] =>
      ResponseLogger.httpApp(true, true)(http)
    }
  }

  val httpApp: HttpApp[F] = loggers(middleware(openRoutes).orNotFound)
}
