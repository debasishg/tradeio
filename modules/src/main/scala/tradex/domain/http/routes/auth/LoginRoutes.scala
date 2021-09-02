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

final case class LoginRoutes[F[_]: JsonDecoder: MonadThrow](
    auth: Auth[F]
) extends Http4sDsl[F] {
  private[routes] val prefixPath = "/auth"

  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "login" =>
      req.decodeR[LoginUser] { user =>
        auth
          .login(user.username, user.password)
          .flatMap(Ok(_))
          .recoverWith {
            // case UserNotFound(_) | InvalidPassword(_) => Forbidden()
            case UserNotFound(_) => {
              println("user not found")
              Forbidden()
            }
            case InvalidPassword(_) => {
              println("invalid password")
              Forbidden()
            }
          }
      }
  }

  val routes: HttpRoutes[F] = Router(
    prefixPath -> httpRoutes
  )
}
