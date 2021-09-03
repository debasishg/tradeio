package tradex.domain
package http.routes

import cats.MonadThrow
import cats.syntax.all._
import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.dsl.Http4sDsl

import org.http4s.server.Router

import repository.AccountRepository

final case class AccountRoutes[F[_]: MonadThrow](
    accountRepository: AccountRepository[F]
) extends Http4sDsl[F] {
  private[routes] val prefixPath = "/accounts"

  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root =>
      accountRepository.all
        .flatMap(Ok(_))
        .recoverWith {
          case th: Throwable => {
            InternalServerError(th.getMessage())
          }
        }
  }

  val routes: HttpRoutes[F] = Router(
    prefixPath -> httpRoutes
  )
}
