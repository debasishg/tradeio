package tradex.domain
package http.routes

import org.typelevel.log4cats.Logger
import cats.MonadThrow
import cats.syntax.all._
import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe.JsonDecoder
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import ext.http4s.refined._

import programs.GenerateTrade
import services.trading.Trading
import Trading._
import services.accounting.Accounting
import tradex.domain.model.trade

final case class GenerateTradeRoutes[F[_]: MonadThrow: JsonDecoder: Logger](
    generateTrade: GenerateTrade[F],
    trading: Trading[F],
    accounting: Accounting[F]
) extends Http4sDsl[F] {
  private[routes] val prefixPath = "/generatetrade"

  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root => {
      req
        .decodeR[trade.GenerateTradeFrontOfficeInput] { tradeParam =>
          GenerateTrade(trading, accounting)
            .generate(tradeParam)
            .flatMap(Created(_))
            .recoverWith {
              case OrderingError(cause) => BadRequest(cause)
              case ExecutionError(cause) => BadRequest(cause)
              case AllocationError(cause) => BadRequest(cause)
              case th: Throwable => {
                th.printStackTrace()
                BadRequest(th.getMessage())
              }
            }
        }
    }
  }

  val routes: HttpRoutes[F] = Router(
    prefixPath -> httpRoutes
  )
}
