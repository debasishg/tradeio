package tradex.domain
package repository
package interpreter
package memory

import java.time.LocalDate
import scala.collection.immutable.Map

import cats._
import cats.data.NonEmptyList
import cats.implicits._
import cats.effect.concurrent.Ref
import cats.effect.Sync

import model.newtypes._

// Constructor private for the interpreter to prevent the Ref from leaking
// access through smart constructor below
final class OrderRepositoryInterpreter[M[_]: Monad] private (
    repo: Ref[M, Map[OrderNo, model.order.Order]]
) extends OrderRepository[M] {
  def query(no: OrderNo): M[Option[model.order.Order]] =
    repo.get.map(_.get(no))

  def queryByOrderDate(date: LocalDate): M[List[model.order.Order]] =
    repo.get.map(_.values.filter(_.date.toLocalDate == date).toList)

  def store(ord: model.order.Order): M[model.order.Order] =
    repo.update(_ + ((ord.no, ord))).map(_ => ord)

  def store(orders: NonEmptyList[model.order.Order]): M[Unit] =
    repo.update(_ ++ orders.toList.map(ord => ((ord.no, ord)))).map(_ => ())
}

// Smart constructor
object OrderRepositoryInterpreter {
  def make[M[_]: Sync]: M[OrderRepositoryInterpreter[M]] =
    Ref
      .of[M, Map[OrderNo, model.order.Order]](Map.empty)
      .map(new OrderRepositoryInterpreter(_))
}
