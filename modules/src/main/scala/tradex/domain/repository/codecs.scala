package tradex.domain
package repository

import skunk._
import skunk.codec.all._

import model.user._
import eu.timepit.refined.types.string.NonEmptyString

object codecs {
  val userId: Codec[UserId] = uuid.imap[UserId](UserId(_))(_.value)
  val userName: Codec[UserName] =
    varchar.imap[UserName](u => UserName(NonEmptyString.unsafeFrom(u)))(
      _.value.value
    )
  val encPassword: Codec[EncryptedPassword] =
    varchar.imap[EncryptedPassword](
      p => EncryptedPassword(NonEmptyString.unsafeFrom(p))
    )(_.value.value)
}
