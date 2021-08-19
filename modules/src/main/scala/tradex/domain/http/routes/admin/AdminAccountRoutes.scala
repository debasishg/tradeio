package tradex.domain
package http.routes.admin

import model.account._
import ext.http4s.refined._
import http.auth.users.AdminUser
import repository.AccountRepository

import cats.MonadThrow
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
      // Create new account
      case ar @ POST -> Root as _ =>
        ar.req.decodeR[CreateAccount] { account =>
          account.toDomain.fold(
            // domain validation failed
            exs => BadRequest(exs.toList.mkString("/")),
            tac =>
              accountRepository
                .store(tac)
                .flatMap { acc =>
                  Created(JsonObject.singleton("account", acc.asJson))
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
