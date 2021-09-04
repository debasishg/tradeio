package tradex.domain
package repository

import cats.syntax.all._
import cats.effect._

import skunk._
import skunk.implicits._

import effects.GenUUID
import model.user._
import model.ID
import codecs._

trait UserRepository[F[_]] {

  /** query by username */
  def query(username: UserName): F[Option[User]]

  /** store a user * */
  def store(username: UserName, password: EncryptedPassword): F[UserId]
}

object UserRepository {
  def make[F[_]: MonadCancelThrow: GenUUID](
      postgres: Resource[F, Session[F]]
  ): UserRepository[F] =
    new UserRepository[F] {
      import UserRepositorySQL._

      def query(userName: UserName): F[Option[User]] =
        postgres.use { session =>
          session.prepare(selectByUserName).use { ps =>
            ps.option(userName)
          }
        }

      def store(username: UserName, password: EncryptedPassword): F[UserId] =
        postgres.use { session =>
          session.prepare(upsertUser).use { cmd =>
            ID.make[F, UserId].flatMap { id =>
              cmd
                .execute(User(id, username, password))
                .as(id)
                .recoverWith { case SqlState.UniqueViolation(_) =>
                  UserNameInUse(username).raiseError[F, UserId]
                }
            }
          }
        }
    }
}

private object UserRepositorySQL {
  val decoder: Decoder[User] =
    (userId ~ userName ~ encPassword)
      .gmap[User]

  val selectByUserName: Query[UserName, User] =
    sql"""
        SELECT u.id, u.name, u.password
        FROM users AS u
        WHERE u.name = $userName
       """.query(decoder)

  val upsertUser: Command[User] =
    sql"""
        INSERT INTO users (id, name, password)
        VALUES ($userId, $userName, $encPassword)
        ON CONFLICT(name) DO UPDATE SET
          password = EXCLUDED.password
       """.command.gcontramap[User]
}
