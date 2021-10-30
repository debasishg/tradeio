package tradex.domain
package repository

import cats.Monad
import skunk._
import AccountRepositorySQL._
import model.account._

object DBUtils {
  case class Paginated[A](as: List[A], more: Boolean)
  val fetchCount = 32

  def queryByNamePattern[F[_]: MonadThrowable](session: Session[F], namePattern: String): F[Paginated[Account]] = {
    val fetchResource =
      session
        .prepare(selectByAccountNamePattern)
        .flatMap { ps =>
          ps.cursor(namePattern).evalMap(_.fetch(fetchCount))
        }
    fetchResource.use { case (accounts, more) =>
      Monad[F].pure(Paginated(accounts, more))
    }
  }
}
