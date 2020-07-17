package tradex.domain
package repository
package interpreter
package memory

import java.time.LocalDateTime
import scala.collection.immutable.Map

import cats.{Order => OrderC, _}
import cats.data.NonEmptyList
import cats.implicits._
import cats.effect.concurrent.Ref
import cats.effect.Sync

import model.order._
import model.newtypes._
import model.enums._

// Constructor private for the interpreter to prevent the Ref from leaking
// access through smart constructor below
final class OrderRepositoryInterpreter[M[_]: Monad] private (
    repo: Ref[M, Map[OrderNo, Order]]
) extends OrderRepository[M] {
  def query(no: OrderNo): M[Option[Order]] =
    repo.get.map(_.get(no))

  def queryByOrderDate(date: LocalDateTime): M[List[Order]] =
    repo.get.map(_.values.filter(_.date == date).toList)

  def store(ord: Order): M[Order] =
    repo.update(_ + ((ord.no, ord))).map(_ => ord)

  def store(orders: NonEmptyList[Order]): M[Unit] =
    repo.update(_ ++ orders.toList.map(ord => ((ord.no, ord)))).map(_ => ())
}

// Smart constructor
object OrderRepositoryInterpreter {
  def make[M[_]: Sync]: M[OrderRepositoryInterpreter[M]] =
    Ref
      .of[M, Map[OrderNo, Order]](Map.empty)
      .map(new OrderRepositoryInterpreter(_))
}
