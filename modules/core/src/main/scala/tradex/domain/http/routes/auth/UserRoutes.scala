package tradex.domain
package http.routes

import model._
import model.user._
import ext.http4s.refined._
import repository.Auth

import cats.MonadThrow
import cats.syntax.all._
import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe.JsonDecoder
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router

final case class UserRoutes[F[_]: JsonDecoder: MonadThrow](
    auth: Auth[F]
) extends Http4sDsl[F] {
  private[routes] val prefixPath = "/auth"

  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] { case req @ POST -> Root / "users" =>
    req
      .decodeR[CreateUser] { user =>
        auth
          .newUser(user.username.toDomain, user.password.toDomain)
          .flatMap(Created(_))
          .recoverWith { case UserNameInUse(u) =>
            Conflict(u.show)
          }
      }
  }

  val routes: HttpRoutes[F] = Router(
    prefixPath -> httpRoutes
  )
}
