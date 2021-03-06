package tradex.domain
package repository

import java.time.LocalDate

import model.enums._

import model.account._

trait AccountRepository[M[_]] {
  /** query by account number */
  def query(no: String): M[Option[Account]]

  /** store */
  def store(a: Account): M[Account]

  /** query by opened date */
  def query(openedOn: LocalDate): M[List[Account]]

  /** all accounts */
  def all: M[List[Account]]

  /** all closed accounts, if date supplied then all closed after that date */
  def allClosed(closeDate: Option[LocalDate]): M[List[Account]]

  /** all accounts trading / settlement / both */
  def allAccountsOfType(accountType: AccountType): M[List[Account]]
}
