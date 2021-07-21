package tradex.domain
package http.routes

import cats.Monad
import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router

import repository.AccountRepository

final case class AccountRoutes[F[_]: Monad](
    accountRepository: AccountRepository[F]
) extends Http4sDsl[F] {
  private[routes] val prefixPath = "/accounts"

  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root =>
      Ok(accountRepository.all)
  }

  val routes: HttpRoutes[F] = Router(
    prefixPath -> httpRoutes
  )
}
