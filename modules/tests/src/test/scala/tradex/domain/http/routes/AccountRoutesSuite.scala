package tradex.domain
package http.routes

import java.time.LocalDate
import cats.effect._
import cats.syntax.all._
import suite.HttpSuite
import repository.AccountRepository
import model.account._

object AccountRoutesSuite extends HttpSuite {
  def dataAccounts(accounts: List[Account]) = new TestAccountRepository {
    override def all: IO[List[Account]] =
      IO.pure(accounts)
    override def query(no: AccountNo): IO[Option[Account]] =
      IO.pure(accounts.find(_.no === no))
    override def query(openedOn: LocalDate): IO[List[Account]] =
      IO.pure(accounts.filter(_.dateOfOpen.equals(openedOn)))
    override def allClosed(closeDate: Option[LocalDate]): IO[List[Account]] =
      IO.pure {
        closeDate
          .map { cd =>
            accounts
              .filter(a =>
                a.dateOfClose.isDefined && a.dateOfClose.get.toLocalDate
                  .isAfter(cd)
              )
              .toList
          }
          .getOrElse {
            accounts.filter(a => a.dateOfClose.isDefined)
          }
      }
  }

  def failingAccounts(accounts: List[Account]) = new TestAccountRepository {
    override def all: IO[List[Account]] =
      IO.raiseError(DummyError) *> IO.pure(accounts)
    override def query(no: AccountNo): IO[Option[Account]] =
      IO.raiseError(DummyError) *> IO.pure(accounts.find(_.no === no))
    override def query(openedOn: LocalDate): IO[List[Account]]              = all
    override def allClosed(closeDate: Option[LocalDate]): IO[List[Account]] = all
  }
}

protected class TestAccountRepository extends AccountRepository[IO] {
  def query(no: AccountNo): IO[Option[Account]]                      = IO.pure(none[Account])
  def store(a: Account): IO[Account]                                 = ???
  def query(openedOn: LocalDate): IO[List[Account]]                  = IO.pure(List.empty[Account])
  def all: IO[List[Account]]                                         = IO.pure(List.empty[Account])
  def allClosed(closeDate: Option[LocalDate]): IO[List[Account]]     = IO.pure(List.empty[Account])
  def allAccountsOfType(accountType: AccountType): IO[List[Account]] = IO.pure(List.empty[Account])
}
