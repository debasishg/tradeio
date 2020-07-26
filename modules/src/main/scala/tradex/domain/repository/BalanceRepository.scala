package tradex.domain
package repository

import java.time.LocalDate

// import cats.data.NonEmptyList
import model.balance._

trait BalanceRepository[M[_]] {
  /** query by account number */
  def query(no: String): M[Option[Balance]]

  /** store */
  def store(a: Balance): M[Balance]

  /** store many balances */
  // def store(balances: NonEmptyList[Balance]): M[Unit]

  /** query all balances that have amount as of this date */
  /** asOf date <= this date */
  def query(date: LocalDate): M[List[Balance]]

  /** all balances */
  def all: M[List[Balance]]
}
