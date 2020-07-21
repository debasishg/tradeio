package tradex.domain.ext

import io.estatico.newtype.Coercible
import io.estatico.newtype.ops._

import cats._
import cats.data._
import cats.effect._

import skunk._
import skunk.data._

object skunkx {
  implicit class CodecOps[B](codec: Codec[B]) {
    def cimap[A: Coercible[B, *]](implicit ev: Coercible[A, B]): Codec[A] =
      codec.imap(_.coerce[A])((ev(_)))
  }

  implicit class SkunkSessionOps[F[_]](session: Session[F])(
      implicit b: Bracket[F, Throwable]
  ) {
    def prepareAndExecute[A](
        arg: A
    )(prepareCommand: Command[arg.type]): F[Completion] =
      session.prepare(prepareCommand).use { cmd =>
        cmd.execute(arg)
      }
  }
}
