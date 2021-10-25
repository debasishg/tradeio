package tradex.domain
package http.routes.secured

import cats.syntax.all._

import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe.JsonDecoder
import org.http4s.dsl.Http4sDsl
import org.http4s.server._
import ext.http4s.refined._

import http.auth.users.CommonUser
import programs.GenerateTrade
import services.trading.Trading
import Trading._
import services.accounting.Accounting
import tradex.domain.model.trade

final case class GenerateTradeRoutes[F[_]: MonadThrowable: JsonDecoder](
    generateTrade: GenerateTrade[F],
    trading: Trading[F],
    accounting: Accounting[F]
) extends Http4sDsl[F] {
  private[routes] val prefixPath = "/generatetrade"

  private val httpRoutes: AuthedRoutes[CommonUser, F] = AuthedRoutes.of {
    case ar @ POST -> Root as user => {
      ar.req
        .decodeR[trade.GenerateTradeFrontOfficeInput] { tradeParam =>
          GenerateTrade(trading, accounting)
            .generate(tradeParam, user.value.userId)
            .flatMap(Created(_))
            .recoverWith {
              case e: TradingError => BadRequest(e.cause)
              case th: Throwable   => BadRequest(th.getMessage())
            }
        }
    }
  }

  def routes(authMiddleware: AuthMiddleware[F, CommonUser]): HttpRoutes[F] =
    Router(
      prefixPath -> authMiddleware(httpRoutes)
    )
}
