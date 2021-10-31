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
  def executeQueryWithCursor(session: Session[IO], namePattern: String, fetchSize: Option[Int] = None): IO[Unit] = {
    def fetchWith(cursor: Cursor[IO, Account]): IO[Unit] = {
      val fetchCount = fetchSize.getOrElse(defaultFetchCount)
      cursor.fetch(fetchCount).flatMap { case (as, more) =>
        as.foreach(a => println(a.no))
        println("-" * 20)
        if (more) fetchWith(cursor)
        else IO.unit
      }
    }
    queryByNamePattern(session, namePattern).use(fetchWith)
  }
}
