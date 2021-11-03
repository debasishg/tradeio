package tradex.domain
package http.routes

import cats.syntax.all._
import cats.effect.kernel.Concurrent

import io.circe.generic.auto._

import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router

import repository.AccountRepository

final case class AccountRoutes[F[_]: Concurrent](
    accountRepository: AccountRepository[F]
) extends Http4sDsl[F] {
  private[routes] val prefixPath     = "/accounts"
  private[routes] val namePrefixPath = "/accountsbyname"

  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root =>
    accountRepository.all
      .flatMap(Ok(_))
      .recoverWith {
        case th: Throwable => {
          InternalServerError(th.getMessage())
        }
      }
  }

  object AccountNameParam extends OptionalQueryParamDecoderMatcher[String]("name")
  private val httpNameRoutes: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root :? AccountNameParam(name) =>
    val accounts = accountRepository
      .query(name.getOrElse("%"))
      .compile
      .toList

    accounts
      .pure[F]
      .flatMap(Ok(_))
      .recoverWith {
        case th: Throwable => {
          InternalServerError(th.getMessage)
        }
      }
  }

  val routes: HttpRoutes[F] = Router(
    prefixPath     -> httpRoutes,
    namePrefixPath -> httpNameRoutes
  )
}
