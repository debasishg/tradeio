package tradex.domain
package repository

import java.time.LocalDateTime

import cats.data.NonEmptyList

import model.newtypes._

import model.trade._

trait TradeRepository[M[_]] {
  /** query by account number and trade date */
  def query(accountNo: AccountNo, date: LocalDateTime): M[List[Trade]]

  /** store */
  def store(trd: Trade): M[Trade]

  /** store many trades */
  def store(trades: NonEmptyList[Trade]): M[Unit]
}
