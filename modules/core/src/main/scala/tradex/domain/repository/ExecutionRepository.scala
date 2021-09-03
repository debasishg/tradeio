package tradex.domain
package repository

import cats.data.NonEmptyList
import cats.syntax.all._
import cats.effect._

import skunk._
import skunk.data.Type
import skunk.codec.all._
import skunk.implicits._

import model.execution._
import model.order.BuySell
import codecs._

trait ExecutionRepository[F[_]] {

  /** store */
  def store(exe: Execution): F[Execution]

  /** store many executions */
  def store(executions: NonEmptyList[Execution]): F[Unit]
}

object ExecutionRepository {
  def make[F[_]: Concurrent](
      postgres: Resource[F, Session[F]]
  ): ExecutionRepository[F] =
    new ExecutionRepository[F] {
      import ExecutionRepositorySQL._

      def store(exe: Execution): F[Execution] =
        postgres.use { session =>
          session.prepare(insertExecution).use { cmd =>
            cmd.execute(exe).void.map(_ => exe)
          }
        }

      def store(executions: NonEmptyList[Execution]): F[Unit] =
        postgres.use { session =>
          session.prepare(insertExecutions(executions.size)).use { cmd =>
            cmd.execute(executions.toList).void.map(_ => ())
          }
        }
    }
}

private object ExecutionRepositorySQL {
  val buySell = enum(BuySell, Type("buysell"))

  val executionEncoder: Encoder[Execution] =
    (executionRefNo ~ accountNo ~ orderNo ~ isinCode ~ market ~ buySell ~ unitPrice ~ quantity ~ timestamp).values
      .gcontramap[Execution]

  val insertExecution: Command[Execution] =
    sql"""
      INSERT INTO executions 
      (
        executionRefNo,
        accountNo,
        orderNo,
        isinCode,
        market,
        buySellFlag,
        unitPrice,
        quantity,
        dateOfExecution
      )
      VALUES $executionEncoder
    """.command

  def insertExecutions(ps: List[Execution]): Command[ps.type] = {
    val enc = executionEncoder.values.list(ps)
    sql"INSERT INTO executions VALUES $enc".command
  }

  def insertExecutions(n: Int): Command[List[Execution]] = {
    val enc = executionEncoder.list(n)
    sql"INSERT INTO executions VALUES $enc".command
  }
}
