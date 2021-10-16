package tradex.domain
package http.routes.admin

import model.account._
import ext.http4s.refined._
import http.auth.users.AdminUser
import repository.AccountRepository

import cats.MonadThrow
import cats.data.NonEmptyList
import cats.syntax.all._
import io.circe.JsonObject
import io.circe.syntax._
import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe.JsonDecoder
import org.http4s.dsl.Http4sDsl
import org.http4s.server._

final case class AdminAccountRoutes[F[_]: JsonDecoder: MonadThrow](
    accountRepository: AccountRepository[F]
) extends Http4sDsl[F] {
  private[admin] val prefixPath = "/accounts"

  private val httpRoutes: AuthedRoutes[AdminUser, F] =
    AuthedRoutes.of {
      // Create new accounts
      // If one create fails, the entire transaction is rolled back
      case ar @ POST -> Root as _ =>
        ar.req.decodeR[List[CreateAccount]] { accounts =>
          accounts
            .map(_.toDomain)
            .sequence
            .fold(
              // domain validation failed
              exs => BadRequest(exs.toList.mkString("/")),
              tacs =>
                accountRepository
                  .store(NonEmptyList.fromList(tacs).get)
                  .flatMap { _ =>
                    Created(JsonObject.singleton("accounts", "Ok".asJson))
                  }
                  .recoverWith {
                    case th: Throwable => {
                      InternalServerError(th.getMessage())
                    }
                  }
            )
        }
    }

  def routes(authMiddleware: AuthMiddleware[F, AdminUser]): HttpRoutes[F] =
    Router(
      prefixPath -> authMiddleware(httpRoutes)
    )
}
