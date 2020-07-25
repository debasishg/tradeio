package tradex.domain
package repository

import java.time.LocalDate

import model.balance._

trait BalanceRepository[M[_]] {
  /** query by account number */
  def query(no: String): M[Option[Balance]]

  /** store */
  def store(a: Balance): M[Balance]

  /** query all balances that have amount as of this date */
  /** asOf date <= this date */
  def query(date: LocalDate): M[List[Balance]]

  /** all balances */
  def all: M[List[Balance]]
}
