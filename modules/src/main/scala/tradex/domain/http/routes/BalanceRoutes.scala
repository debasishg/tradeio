package tradex.domain
package http.routes

import cats.Monad
import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router

import repository.BalanceRepository

final case class BalanceRoutes[F[_]: Monad](
    balanceRepository: BalanceRepository[F]
) extends Http4sDsl[F] {
  private[routes] val prefixPath = "/balances"

  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root =>
      Ok(balanceRepository.all)
  }

  val routes: HttpRoutes[F] = Router(
    prefixPath -> httpRoutes
  )
}
