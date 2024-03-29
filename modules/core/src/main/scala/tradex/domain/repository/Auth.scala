package tradex.domain
package repository

import auth.{ Crypto, Tokens }
import config.config.TokenExpiration
import model._
import model.user._
import http.auth.users.{ User => AUser, _ }
import cats._
import cats.syntax.all._
import dev.profunktor.auth.jwt.JwtToken
import io.circe.parser.decode
import io.circe.syntax._
import pdi.jwt.JwtClaim
import dev.profunktor.redis4cats.RedisCommands

trait Auth[F[_]] {
  def newUser(username: UserName, password: Password): F[JwtToken]
  def login(username: UserName, password: Password): F[JwtToken]
  def logout(token: JwtToken, username: UserName): F[Unit]
}

trait UsersAuth[F[_], A] {
  def findUser(token: JwtToken)(claim: JwtClaim): F[Option[A]]
}

object UsersAuth {
  def admin[F[_]: Applicative](
      adminToken: JwtToken,
      adminUser: AdminUser
  ): UsersAuth[F, AdminUser] =
    new UsersAuth[F, AdminUser] {
      def findUser(token: JwtToken)(claim: JwtClaim): F[Option[AdminUser]] =
        (token === adminToken)
          .guard[Option]
          .as(adminUser)
          .pure[F]
    }

  def common[F[_]: Functor](
      redis: RedisCommands[F, String, String]
  ): UsersAuth[F, CommonUser] =
    new UsersAuth[F, CommonUser] {
      def findUser(token: JwtToken)(claim: JwtClaim): F[Option[CommonUser]] = {
        redis
          .get(token.value)
          .map {
            _.flatMap { u =>
              decode[AUser](u).toOption.map(CommonUser.apply)
            }
          }
      }
    }
}

object Auth {
  def make[F[_]: MonadThrow](
      tokenExpiration: TokenExpiration,
      tokens: Tokens[F],
      users: UserRepository[F],
      redis: RedisCommands[F, String, String],
      crypto: Crypto
  ): Auth[F] =
    new Auth[F] {
      private val TokenExpiration = tokenExpiration.value

      def newUser(username: UserName, password: Password): F[JwtToken] =
        users.query(username).flatMap {
          case Some(_) => UserNameInUse(username).raiseError[F, JwtToken]
          case None =>
            for {
              i <- users.store(username, crypto.encrypt(password))
              t <- tokens.create
              u = AUser(i, username).asJson.noSpaces
              _ <- redis.setEx(t.value, u, TokenExpiration)
              _ <- redis.setEx(username.show, t.value, TokenExpiration)
            } yield t
        }

      def login(username: UserName, password: Password): F[JwtToken] =
        users.query(username).flatMap {
          case None => UserNotFound(username).raiseError[F, JwtToken]
          case Some(user) if user.password =!= crypto.encrypt(password) =>
            InvalidPassword(user.userName).raiseError[F, JwtToken]
          case Some(user) =>
            redis.get(username.show).flatMap {
              case Some(t) => JwtToken(t).pure[F]
              case None =>
                tokens.create.flatTap { t =>
                  redis
                    .setEx(t.value, user.asJson.noSpaces, TokenExpiration) *>
                    redis.setEx(username.show, t.value, TokenExpiration)
                }
            }
        }

      def logout(token: JwtToken, username: UserName): F[Unit] = {
        redis.del(token.show) *> redis.del(username.show).void
      }
    }
}
