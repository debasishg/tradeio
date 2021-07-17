package tradex.domain
package repository
package interpreter
package memory

import java.time.LocalDate
import scala.collection.immutable.Map

import cats._
import cats.data.NonEmptyList
import cats.syntax.all._
import cats.effect.Ref
import cats.effect.Sync

// Constructor private for the interpreter to prevent the Ref from leaking
// access through smart constructor below
final class OrderRepositoryInterpreter[M[_]: Monad] private (
    repo: Ref[M, Map[String, model.order.Order]]
) extends OrderRepository[M] {
  def query(no: String): M[Option[model.order.Order]] =
    repo.get.map(_.get(no))

  def queryByOrderDate(date: LocalDate): M[List[model.order.Order]] =
    repo.get.map(_.values.filter(_.date.toLocalDate == date).toList)

  def store(ord: model.order.Order): M[model.order.Order] =
    repo.update(_ + ((ord.no.value.value, ord))).map(_ => ord)

  def store(orders: NonEmptyList[model.order.Order]): M[Unit] =
    repo
      .update(_ ++ orders.toList.map(ord => ((ord.no.value.value, ord))))
      .map(_ => ())
}

// Smart constructor
object OrderRepositoryInterpreter {
  def make[M[_]: Sync]: M[OrderRepositoryInterpreter[M]] =
    Ref
      .of[M, Map[String, model.order.Order]](Map.empty)
      .map(new OrderRepositoryInterpreter(_))
}
