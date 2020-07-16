package org.github.domain
package repository
package interpreter.skunk

import java.time.LocalDateTime

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
import model.trade._
import model.market._
import ext.skunkx._

final class TradeRepositoryInterpreter[M[_]: Sync] private (
  sessionPool: Resource[M, Session[M]]) extends TradeRepository[M] {

  import TradeQueries._

  def query(accountNo: AccountNo, date: LocalDateTime): M[List[Trade]] = 
    sessionPool.use { session =>
      session.prepare(selectByAccountNoAndDate).use { ps =>
        ps.stream(accountNo ~ date, 1024).compile.toList
      }
    }

  def store(exe: Trade): M[Trade] = 
    sessionPool.use { session =>
      session.prepare(insertTrade).use { cmd =>
        cmd.execute(exe).void.map(_ => exe)
      }
    }

  def store(executions: NonEmptyList[Trade]): M[Unit] =
    sessionPool.use { session =>
      session.prepare(insertExecutions(executions.size)).use { cmd =>
        cmd.execute(executions.toList).void.map(_ => ())
      }
    }
}
  
private object TradeQueries {

  val buySell = enum(BuySell, Type("buysellflag"))
  implicit val moneyContext = defaultMoneyContext

  val tradeDecoder: Decoder[Trade] = 
    (varchar ~ varchar ~ varchar ~ varchar ~ buySell ~ numeric ~ numeric ~ timestamp ~ timestamp.opt ~ numeric.opt).map {
      case rno ~ ano ~ isin ~ mkt ~ bs ~ up ~ qty ~ td ~ vd ~ na =>
        Trade(
          AccountNo(ano),
          ISINCode(isin),
          TradeReferenceNo(rno),
          Market.withName(mkt),
          bs,
          UnitPrice(up),
          Quantity(qty),
          td,
          vd,
          None,
          na.map(Money(_))
        )
    }

  val tradeEncoder: Encoder[Trade] = 
    (varchar ~ varchar ~ varchar ~ varchar ~ buySell ~ numeric ~ numeric ~ timestamp ~ timestamp.opt ~ numeric.opt)
      .values.contramap((t: Trade) => 
        t.refNo.value ~ t.accountNo.value ~ t.isin.value ~ t.market.toString ~ t.buySell ~ t.unitPrice.value ~ t.quantity.value ~ t.tradeDate ~ t.valueDate ~ t.netAmount.map(_.value))

  val insertTrade: Command[Trade] =
    sql"""
      INSERT INTO trades 
      (
        tradeRefNo,
        accountNo,
        isinCode,
        market,
        buySellFlag,
        unitPrice,
        quantity,
        tradeDate,
        valueDate,
        netAmount
      )
      VALUES $tradeEncoder
    """.command

  def insertTrades(ps: List[Trade]): Command[ps.type] = {
    val enc = tradeEncoder.values.list(ps)
    sql"INSERT INTO trades VALUES $enc".command
  }

  def insertExecutions(n: Int): Command[List[Trade]] = {
    val enc = tradeEncoder.list(n)
    sql"INSERT INTO trades VALUES $enc".command
  }

  val selectByAccountNoAndDate: Query[AccountNo ~ LocalDateTime, Trade] =
    sql"""
        SELECT t.tradeRefNo, t.accountNo, t.isinCode, t.market, t.buySellFlag, t.unitPrice, t.quantity, t.tradeDate, t.valueDate, t.netAmount
        FROM trades AS t
        WHERE t.accountNo = ${varchar.cimap[AccountNo]}
          AND t.tradeDate = $timestamp
    """.query(tradeDecoder)

}