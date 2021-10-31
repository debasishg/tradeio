package tradex.domain
package repository

import cats.effect._
import skunk._
import AccountRepositorySQL._
import model.account._

object DBUtils {
  case class Paginated[A](as: List[A], more: Boolean)
  val defaultFetchCount = 16

  // a sample query that fetches based on a specified name pattern (LIKE query in SQL)
  // this API returns a `Cursor`, which can be traversed through in the user level API
  def queryByNamePattern[F[_]](session: Session[F], namePattern: String): Resource[F, Cursor[F, Account]] =
    session
      .prepare(selectByAccountNamePattern)
      .flatMap { pq =>
        pq.cursor(namePattern)
      }

  // demonstrates usage of a cursor
  def queryByNamePatternPaged[A](
      session: Session[IO],
      namePattern: String,
      cont: List[Account] => Paginated[A],
      fetchSize: Option[Int] = None
  ): IO[Int] = {
    def fetchWith(cursor: Cursor[IO, Account], pageCount: Int): IO[Int] = {
      val fetchCount = fetchSize.getOrElse(defaultFetchCount)
      cursor.fetch(fetchCount).flatMap { case (as, more) =>
        if (cont(as).more && more) fetchWith(cursor, pageCount + 1)
        else IO(pageCount)
      }
    }
    queryByNamePattern(session, namePattern).use(c => fetchWith(c, 0))
  }
}
