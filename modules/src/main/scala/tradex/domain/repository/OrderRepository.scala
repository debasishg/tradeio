package tradex.domain
package repository

import java.time.LocalDate

import cats.data.NonEmptyList

import model.order._

trait OrderRepository[M[_]] {
  /** query by unique key order no, account number and date */
  def query(no: String): M[Option[Order]]

  /** query by order date */
  def queryByOrderDate(date: LocalDate): M[List[Order]]

  /** store */
  def store(ord: Order): M[Order]

  /** store many orders */
  def store(orders: NonEmptyList[Order]): M[Unit]
}
