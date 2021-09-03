package tradex.domain
package repository
package interpreter
package memory

import scala.collection.immutable.Map

import cats._
import cats.data.NonEmptyList
import cats.syntax.all._
import cats.effect.Ref
import cats.effect.Sync

import model.account.AccountNo
import model.instrument.ISINCode
import model.execution._

// Constructor private for the interpreter to prevent the Ref from leaking
// access through smart constructor below
final class ExecutionRepositoryInterpreter[M[_]: Monad] private (
    repo: Ref[M, Map[(AccountNo, ISINCode, ExecutionReferenceNo), Execution]]
) extends ExecutionRepository[M] {
  def store(exe: Execution): M[Execution] =
    repo
      .update(_ + (((exe.accountNo, exe.isin, exe.executionRefNo), exe)))
      .map(_ => exe)

  def store(executions: NonEmptyList[Execution]): M[Unit] =
    repo
      .update(
        _ ++ executions.toList
          .map(exe => (((exe.accountNo, exe.isin, exe.executionRefNo), exe)))
      )
      .map(_ => ())
}

// Smart constructor
object ExecutionRepositoryInterpreter {
  def make[M[_]: Sync]: M[ExecutionRepositoryInterpreter[M]] =
    Ref
      .of[M, Map[(AccountNo, ISINCode, ExecutionReferenceNo), Execution]](
        Map.empty
      )
      .map(new ExecutionRepositoryInterpreter(_))
}
