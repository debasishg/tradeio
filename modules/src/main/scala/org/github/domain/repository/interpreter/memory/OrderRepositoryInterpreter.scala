package org.github.domain
package repository
package interpreter
package memory

import java.time.LocalDateTime
import scala.collection.immutable.Map 

import cats.{Order => OrderC, _}
import cats.implicits._
import cats.effect.concurrent.Ref
import cats.effect.Sync

import common._
import model.order._
import model.newtypes._
import model.enums._

// Constructor private for the interpreter to prevent the Ref from leaking
// access through smart constructor below
final class OrderRepositoryInterpreter[M[_]: Monad] private (repo: Ref[M, Map[(OrderNo, AccountNo, LocalDateTime), Order]])
  extends OrderRepository[M] {

  def query(no: OrderNo, accountNo: AccountNo, date: LocalDateTime): M[Option[Order]] = 
    repo.get.map(_.get((no, accountNo, date)))

  def queryByOrderDate(date: LocalDateTime): M[List[Order]] = 
    repo.get.map(_.values.filter(_.date == date).toList)

  def store(ord: Order): M[Order] = 
    repo.update(_ + (((ord.no, ord.accountNo, ord.date), ord))).map(_ => ord)
}

// Smart constructor 
object OrderRepositoryInterpreter {
  def make[M[_]: Sync]: M[OrderRepositoryInterpreter[M]] =
    Ref.of[M, Map[(OrderNo, AccountNo, LocalDateTime), Order]](Map.empty).map(new OrderRepositoryInterpreter(_))
}