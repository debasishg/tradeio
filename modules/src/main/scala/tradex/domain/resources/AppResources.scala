package tradex.domain
package resources

import cats.effect._
import cats.effect.std.Console
import cats.syntax.all._

import fs2.io.net.Network
import org.typelevel.log4cats.Logger

import skunk._
import skunk.util.Typer
import skunk.codec.text._
import skunk.implicits._

import org.http4s.client.Client

import natchez.Trace.Implicits.noop // needed for skunk

import config.config._

sealed abstract class AppResources[F[_]] private (
    val client: Client[F],
    val postgres: Resource[F, Session[F]]
)

object AppResources {
  def make[F[_]: Concurrent: Network: Console: MkHttpClient: Logger](
      cfg: AppConfig
  ): Resource[F, AppResources[F]] = {
    def checkPostgresConnection(
        postgres: Resource[F, Session[F]]
    ): F[Unit] =
      postgres.use { session =>
        session.unique(sql"select version();".query(text)).flatMap { v =>
          Logger[F].info(s"Connected to Postgres $v")
        }
      }

    def mkPostgreSqlResource(c: PostgreSQLConfig): SessionPool[F] =
      Session
        .pooled[F](
          host = c.host.value,
          port = c.port.value,
          user = c.user.value,
          database = c.database.value,
          max = c.max.value,
          strategy = Typer.Strategy.SearchPath
        )
        .evalTap(checkPostgresConnection)

    (
      MkHttpClient[F].newEmber(cfg.httpClientConfig),
      mkPostgreSqlResource(cfg.postgreSQL)
    ).parMapN(new AppResources[F](_, _) {})
  }
}
