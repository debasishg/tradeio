package org.github.domain
package repository
package interpreter.skunk

import java.time.LocalDateTime

import cats.data.NonEmptyList
import cats.implicits._
import cats.effect._
import cats.effect.concurrent.Ref

import skunk._
import skunk.data.Type
import skunk.codec.all._
import skunk.implicits._

import squants.market._
import squants.market.defaultMoneyContext

import model.newtypes._
import model.enums._
import model.order._
import Order._
import ext.skunkx._

final class OrderRepositoryInterpreter[M[_]: Sync] private (
  sessionPool: Resource[M, Session[M]]) extends OrderRepository[M] {
  def query(no: OrderNo, accountNo: AccountNo, date: LocalDateTime): M[Option[Order]] = ???
  def queryByOrderDate(date: LocalDateTime): M[List[Order]] = ???
  def store(ord: Order): M[Order] = ???
  def store(orders: NonEmptyList[Order]): M[Unit] = ???
}
  
private object OrderQueries {

}