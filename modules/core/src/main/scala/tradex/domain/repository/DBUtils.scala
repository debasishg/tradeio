package tradex.domain
package repository

import cats.Monad
import cats.effect._
import skunk._
import AccountRepositorySQL._
import model.account._

object DBUtils {
  case class Paginated[A](as: List[A], more: Boolean)
  val fetchCount = 16

  def queryByNamePattern[F[_]](session: Session[F], namePattern: String): Resource[F, Cursor[F, Account]] =
    session
      .prepare(selectByAccountNamePattern)
      .flatMap { ps =>
        ps.cursor(namePattern)
      }

  def executeQueryWithCursor(session: Session[IO], namePattern: String): IO[Unit] = {
    def fetchWith(cursor: Cursor[IO, Account]): IO[Unit] = {
      cursor.fetch(fetchCount).flatMap { case (as, m) =>
        as.foreach(a => println(a.no))
        println("-" * 20)
        if (m) fetchWith(cursor)
        else IO.unit
      }
    }
    queryByNamePattern(session, namePattern).use(fetchWith)
  }
}
