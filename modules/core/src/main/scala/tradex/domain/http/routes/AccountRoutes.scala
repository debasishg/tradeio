package tradex.domain
package http.routes

import cats.MonadThrow
import cats.syntax.all._

import io.circe.generic.auto._
import io.circe.syntax._

import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router

import fs2.Stream

import repository.AccountRepository

final case class AccountRoutes[F[_]: MonadThrow](
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

  private val httpNameRoutes: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root =>
    val prefix = Stream.eval("[".pure[F])
    val suffix = Stream.eval("]".pure[F])

    val accs = accountRepository
      .query("%")
      .groupAdjacentBy(_.no)
      .map { case (_, rows) => rows.toList }
      .map(_.asJson.noSpaces)
      .intersperse(",")

    val result: Stream[F, String] = prefix ++ accs ++ suffix
    Ok(result)
  }

  val routes: HttpRoutes[F] = Router(
    prefixPath     -> httpRoutes,
    namePrefixPath -> httpNameRoutes
  )
}
