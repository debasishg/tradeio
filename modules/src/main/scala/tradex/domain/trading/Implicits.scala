package tradex.domain
package trading

import cats._
import cats.mtl._

object Implicits {
  object DefaultApplicativeAsk {
    def constant[F[_]: Applicative, E](e: E): ApplicativeAsk[F, E] = {
      new DefaultApplicativeAsk[F, E] {
        val applicative: Applicative[F] = Applicative[F]
        def ask: F[E] = applicative.pure(e)
      }
    }
  }

  object TailRecM {
    def defaultTailRecM[F[_], A, B](
        a: A
    )(f: A => F[Either[A, B]])(implicit F: Monad[F]): F[B] =
      F.flatMap(f(a)) {
        case Left(a2) => defaultTailRecM(a2)(f)
        case Right(b) => F.pure(b)
      }
  }
}
