package tradex.domain
package repository

import cats.syntax.all._
import cats.effect._

import skunk._
import skunk.codec.all._
import skunk.implicits._

import effects.GenUUID
import model.user._
import model.ID

trait UserRepository[F[_]] {
  /** query by username */
  def query(username: UserName): F[Option[User]]

  /** store a user **/
  def store(username: UserName, password: EncryptedPassword): F[UserId]
}

object UserRepository {
  def make[F[_]: Concurrent: GenUUID](
      postgres: Resource[F, Session[F]]
  ): UserRepository[F] =
    new UserRepository[F] {
      import UserRepositorySQL._

      def query(userName: UserName): F[Option[User]] =
        postgres.use { session =>
          session.prepare(selectByUserName).use { ps =>
            ps.option(userName.value.value)
          }
        }

      def store(username: UserName, password: EncryptedPassword): F[UserId] =
        postgres.use { session =>
          session.prepare(upsertUser).use { cmd =>
            ID.make[F, UserId].flatMap { id =>
              cmd
                .execute(User(id, username, password))
                .void
                .map(_ => id)
            }
          }
        }
    }
}

private object UserRepositorySQL {
  val decoder: Decoder[User] =
    (uuid ~ varchar ~ varchar)
      .map {
        case id ~ nm ~ pd =>
          User
            .user(id, nm, pd)
            .fold(
              exs => throw new Exception(exs.toList.mkString("/")),
              identity
            )
      }

  // val userCodec = uuid ~ varchar ~ varchar
  val encoder: Encoder[User] =
    (uuid ~ varchar ~ varchar).values
      .contramap(
        (u: User) =>
          u.userId.value ~ u.userName.value.value ~ u.password.value.value
      )

  val selectByUserName: Query[String, User] =
    sql"""
        SELECT u.id, u.name, u.password
        FROM users AS u
        WHERE u.name = $varchar
       """.query(decoder)

  val upsertUser: Command[User] =
    sql"""
        INSERT INTO users (id, name, password)
        VALUES ($uuid, $varchar, $varchar)
        ON CONFLICT(name) DO UPDATE SET
          password    = EXCLUDED.password
       """.command.contramap {
      case u =>
        u match {
          case User(id, name, passwd) =>
            id.value ~ name.value.value ~ passwd.value.value
        }
    }
}
