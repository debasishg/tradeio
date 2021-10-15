package tradex.domain
package http.routes.admin

import model.instrument._
import ext.http4s.refined._
import http.auth.users.AdminUser
import repository.InstrumentRepository

import cats.MonadThrow
import cats.syntax.all._
import io.circe.JsonObject
import io.circe.syntax._
import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe.JsonDecoder
import org.http4s.dsl.Http4sDsl
import org.http4s.server._

final case class AdminInstrumentRoutes[F[_]: JsonDecoder: MonadThrow](
    instrumentRepository: InstrumentRepository[F]
) extends Http4sDsl[F] {
  private[admin] val prefixPath = "/instruments"

  private val httpRoutes: AuthedRoutes[AdminUser, F] =
    AuthedRoutes.of {
      // Create new instrument
      case ar @ POST -> Root as _ =>
        ar.req.decodeR[CreateInstrument] { instrument =>
          instrument.toDomain.fold(
            // domain validation failed
            exs => BadRequest(exs.toList.mkString("/")),
            tins =>
              instrumentRepository
                .store(tins)
                .flatMap { ins =>
                  Created(JsonObject.singleton("instrument", ins.asJson))
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
