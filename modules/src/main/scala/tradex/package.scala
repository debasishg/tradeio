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

  object DefaultApplicativeAsk {
    def constant[F[_]: Applicative, E](e: E): ApplicativeAsk[F, E] = {
      new DefaultApplicativeAsk[F, E] {
        val applicative: Applicative[F] = Applicative[F]
        def ask: F[E] = applicative.pure(e)
      }
    }
  }

  def askRepo[A](repo: A) = DefaultApplicativeAsk.constant[IO, A](repo)

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
