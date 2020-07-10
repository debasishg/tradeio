package org.github.domain
package repository

import java.time.LocalDateTime

import model.newtypes._

import model.order._

trait OrderRepository[M[_]] {
  /** query by unique key order no, account number and date */
  def query(no: OrderNo, accountNo: AccountNo, date: LocalDateTime): M[Option[Order]]
  /** query by order date */
  def queryByOrderDate(date: LocalDateTime): M[List[Order]]
  /** store */
  def store(ord: Order): M[Order]
}
