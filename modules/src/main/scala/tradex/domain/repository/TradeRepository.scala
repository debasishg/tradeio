package tradex.domain
package repository

import java.time.LocalDate

import cats.data.NonEmptyList

import model.newtypes._

import model.trade._

trait TradeRepository[M[_]] {
  /** query by account number and trade date (compares using the date part only) */
  def query(accountNo: AccountNo, date: LocalDate): M[List[Trade]]

  /** store */
  def store(trd: Trade): M[Trade]

  /** store many trades */
  def store(trades: NonEmptyList[Trade]): M[Unit]
}
