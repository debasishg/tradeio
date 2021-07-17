package tradex.domain
package repository
package interpreter
package memory

import java.time.LocalDate
import scala.collection.immutable.Map

import cats._
import cats.syntax.all._
import cats.effect.Ref
import cats.effect.Sync

import model.account._
import model.enums._

// Constructor private for the interpreter to prevent the Ref from leaking
// access through smart constructor below
final class AccountRepositoryInterpreter[M[_]: Monad] private (
    repo: Ref[M, Map[String, Account]]
) extends AccountRepository[M] {
  def query(no: String): M[Option[Account]] = repo.get.map(_.get(no))

  def store(a: Account): M[Account] =
    repo.update(_ + ((a.no.value.value, a))).map(_ => a)

  def query(openedOn: LocalDate): M[List[Account]] =
    repo.get.map(_.values.filter(_.dateOfOpen.toLocalDate == openedOn).toList)

  def all: M[List[Account]] = repo.get.map(_.values.toList)

  def allClosed(closeDate: Option[LocalDate]): M[List[Account]] =
    closeDate
      .map { cd =>
        repo.get.map(
          _.values
            .filter(
              a =>
                a.dateOfClose.isDefined && a.dateOfClose.get.toLocalDate
                  .isAfter(cd)
            )
            .toList
        )
      }
      .getOrElse {
        repo.get.map(_.values.filter(a => a.dateOfClose.isDefined).toList)
      }

  def allAccountsOfType(accountType: AccountType): M[List[Account]] =
    repo.get.map(_.values.filter(_.accountType == accountType).toList)
}

// Smart constructor
object AccountRepositoryInterpreter {
  def make[M[_]: Sync]: M[AccountRepositoryInterpreter[M]] =
    Ref
      .of[M, Map[String, Account]](Map.empty)
      .map(new AccountRepositoryInterpreter(_))
}
