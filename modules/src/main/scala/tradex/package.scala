package tradex

import java.time.LocalDateTime

import cats._
import cats.data._
import cats.mtl._
import cats.implicits._
import cats.effect.IO

import squants.market._

import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.auto._

package object domain {
  type MonadThrowable[F[_]] = MonadError[F, Throwable]

  def today = LocalDateTime.now
  final val ZERO_BIG_DECIMAL = BigDecimal(0)

  implicit val moneyContext = defaultMoneyContext

  def askRepo[A](repo: A) = Ask.const[IO, A](repo)

  object NewtypeRefinedOps {
    import io.estatico.newtype.Coercible
    import io.estatico.newtype.ops._

    final class NewtypeRefinedPartiallyApplied[A] {
      def apply[T, P](raw: T)(
          implicit c: Coercible[Refined[T, P], A],
          v: Validate[T, P]
      ): EitherNec[String, A] =
        refineV[P](raw).toEitherNec.map(_.coerce[A])
    }

    def validate[A]: NewtypeRefinedPartiallyApplied[A] =
      new NewtypeRefinedPartiallyApplied[A]
  }
}
