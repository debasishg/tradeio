package tradex.domain
package http.auth

import model.user._

import derevo.cats.show
import derevo.circe.magnolia.{ decoder, encoder }
import derevo.derive
import dev.profunktor.auth.jwt._
import io.estatico.newtype.macros.newtype

object users {
  @newtype case class AdminJwtAuth(value: JwtSymmetricAuth)
  @newtype case class UserJwtAuth(value: JwtSymmetricAuth)

  @derive(decoder, encoder, show)
  case class User(userId: UserId, userName: UserName)

  @derive(decoder, encoder)
  case class UserWithPassword(
      id: UserId,
      name: UserName,
      password: EncryptedPassword
  )

  @derive(show)
  @newtype
  case class CommonUser(value: User)

  @derive(show)
  @newtype
  case class AdminUser(value: User)
}
