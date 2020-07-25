package tradex.domain
package repository
package interpreter
package memory

import java.time.LocalDate
import scala.collection.immutable.Map

import cats._
import cats.implicits._
import cats.effect.concurrent.Ref
import cats.effect.Sync

import model.balance._

// Constructor private for the interpreter to prevent the Ref from leaking
// access through smart constructor below
final class BalanceRepositoryInterpreter[M[_]: Monad] private (
    repo: Ref[M, Map[String, Balance]]
) extends BalanceRepository[M] {
  def query(no: String): M[Option[Balance]] = repo.get.map(_.get(no))

  def store(b: Balance): M[Balance] =
    repo.update(_ + ((b.accountNo.value.value, b))).map(_ => b)

  def query(date: LocalDate): M[List[Balance]] =
    repo.get.map(_.values.filter(_.asOf.toLocalDate.isBefore(date)).toList)

  def all: M[List[Balance]] = repo.get.map(_.values.toList)
}

// Smart constructor
object BalanceRepositoryInterpreter {
  def make[M[_]: Sync]: M[BalanceRepositoryInterpreter[M]] =
    Ref
      .of[M, Map[String, Balance]](Map.empty)
      .map(new BalanceRepositoryInterpreter(_))
}
