package tradex.domain
package http.routes

import cats.Monad
import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router

import model.market._
import repository.TradeRepository

final case class TradeRoutes[F[_]: Monad](
    tradeRepository: TradeRepository[F]
) extends Http4sDsl[F] {
  private[routes] val prefixPath = "/trades"

  object MarketQueryParam
      extends OptionalQueryParamDecoderMatcher[MarketParam]("market")

  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root :? MarketQueryParam(market) =>
      Ok(
        market.fold(tradeRepository.all)(
          m => tradeRepository.queryByMarket(m.toDomain)
        )
      )
  }

  val routes: HttpRoutes[F] = Router(
    prefixPath -> httpRoutes
  )
}
