package org.github.domain
package repository
package interpreter.skunk

import cats.data.NonEmptyList
import cats.implicits._
import cats.effect._

import skunk._
import skunk.data.Type
import skunk.codec.all._
import skunk.implicits._

import squants.market._

import model.newtypes._
import model.enums._
import model.execution._
import ext.skunkx._

final class ExecutionRepositoryInterpreter[M[_]: Sync] private (
  sessionPool: Resource[M, Session[M]]) extends ExecutionRepository[M] {

  import ExecutionQueries._

  def store(exe: Execution): M[Execution] = 
    sessionPool.use { session =>
      session.prepare(insertExecution).use { cmd =>
        cmd.execute(exe).void.map(_ => exe)
      }
    }

  def store(executions: NonEmptyList[Execution]): M[Unit] =
    sessionPool.use { session =>
      session.prepare(insertExecutions(executions.size)).use { cmd =>
        cmd.execute(executions.toList).void.map(_ => ())
      }
    }
}
  
private object ExecutionQueries {

  val buySell = enum(BuySell, Type("buysellflag"))
  implicit val moneyContext = defaultMoneyContext

  val executionEncoder: Encoder[Execution] = 
    (varchar ~ varchar ~ varchar ~ varchar ~ varchar ~ buySell ~ numeric ~ numeric ~ timestamp)
      .values.contramap((e: Execution) => 
        e.executionRefNo.value ~ e.accountNo.value ~ e.orderNo.value ~ e.isin.value ~ e.market.toString ~ e.buySell ~ e.unitPrice.value ~ e.quantity.value ~ e.dateOfExecution)

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

// Smart constructor 
object ExecutionRepositoryInterpreter {
  def make[M[_]: Sync](
    sessionPool: Resource[M, Session[M]]
  ): M[ExecutionRepositoryInterpreter[M]] = Sync[M].delay(new ExecutionRepositoryInterpreter[M](sessionPool))
}
