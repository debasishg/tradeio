package tradex.domain
package model

import java.util.UUID
import javax.crypto.Cipher
import scala.util.control.NoStackTrace
import cats.data.ValidatedNec
import cats.syntax.all._

import derevo.cats._
import derevo.circe.magnolia.{ decoder, encoder }
import derevo.derive
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe._
import io.circe.refined._
import io.estatico.newtype.macros.newtype
import optics.uuid

import NewtypeRefinedOps._

object user {
  @derive(decoder, encoder, eqv, show, uuid)
  @newtype
  case class UserId(value: UUID)

  @derive(decoder, encoder, eqv, show)
  @newtype
  case class UserName(value: NonEmptyString)

  @derive(decoder, encoder, eqv, show)
  @newtype
  case class Password(value: NonEmptyString)

  @derive(decoder, encoder, eqv, show)
  @newtype
  case class EncryptedPassword(value: NonEmptyString)

  @newtype
  case class EncryptCipher(value: Cipher)

  @newtype
  case class DecryptCipher(value: Cipher)

  case class UserNotFound(username: UserName)    extends NoStackTrace
  case class UserNameInUse(username: UserName)   extends NoStackTrace
  case class InvalidPassword(username: UserName) extends NoStackTrace
  case object UnsupportedOperation               extends NoStackTrace
  case object TokenNotFound                      extends NoStackTrace

  @derive(decoder, encoder, eqv, show)
  private[domain] final case class User private (
      userId: UserId,
      userName: UserName,
      password: EncryptedPassword
  )

  object User {
    def user(
        id: UUID,
        name: String,
        password: String
    ): ValidatedNec[String, User] = {
      (
        validateUserName(name),
        validatePassword(password)
      ).mapN { (nm, pd) =>
        User(UserId(id), nm, pd)
      }
    }

    private[model] def validateUserName(
        name: String
    ): ValidatedNec[String, UserName] =
      validate[UserName](name)
        .leftMap(_ :+ s"User Name cannot be blank")

    private[model] def validatePassword(
        name: String
    ): ValidatedNec[String, EncryptedPassword] =
      validate[EncryptedPassword](name)
        .leftMap(_ :+ s"User Password cannot be blank")
  }

  // --------- user registration -----------

  @derive(decoder, encoder)
  @newtype
  case class UserNameParam(value: NonEmptyString) {
    def toDomain: UserName = UserName(value)
  }

  @derive(decoder, encoder)
  @newtype
  case class PasswordParam(value: NonEmptyString) {
    def toDomain: Password = Password(value)
  }

  @derive(decoder, encoder)
  case class CreateUser(
      username: UserNameParam,
      password: PasswordParam
  )

  // --------- user login -----------

  @derive(decoder, encoder)
  case class LoginUser(
      username: UserName,
      password: Password
  )

  // --------- admin auth -----------

  @newtype
  case class ClaimContent(uuid: UUID)

  object ClaimContent {
    implicit val jsonDecoder: Decoder[ClaimContent] =
      Decoder.forProduct1("uuid")(ClaimContent.apply)
  }
}
