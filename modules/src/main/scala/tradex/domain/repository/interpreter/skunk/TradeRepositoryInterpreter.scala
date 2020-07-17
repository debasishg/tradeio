package tradex.domain
package repository
package interpreter.skunk

import java.time.LocalDateTime

import cats.Semigroup
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
import common._

final class TradeRepositoryInterpreter[M[_]: Sync] private (
    sessionPool: Resource[M, Session[M]]
) extends TradeRepository[M] {
  import TradeQueries._

  def query(accountNo: AccountNo, date: LocalDateTime): M[List[Trade]] =
    sessionPool.use { session =>
      session.prepare(selectByAccountNoAndDate).use { ps =>
        ps.stream(accountNo ~ date, 1024)
          .compile
          .toList
          .map(_.groupBy(_._2))
          .map { m =>
            m.map {
              case (refNo, lis) =>
                val singleTradeTaxFeeLine =
                  makeSingleTradeTaxFees(TradeReferenceNo(refNo), lis)
                singleTradeTaxFeeLine.tail.foldLeft(singleTradeTaxFeeLine.head)(
                  Semigroup[Trade].combine
                )
            }.toList
          }
      }
    }

  private def makeSingleTradeTaxFees(
      trdRefNo: TradeReferenceNo,
      trdTxs: List[
        String ~
          String ~
          String ~
          BuySell ~
          BigDecimal ~
          BigDecimal ~
          LocalDateTime ~
          Option[LocalDateTime] ~
          Option[BigDecimal] ~
          TaxFeeId ~
          BigDecimal ~
          String
      ]
  ): List[Trade] = {
    trdTxs.map {
      case ano ~ isin ~ mkt ~ bs ~ up ~ qty ~ td ~ vd ~ na ~ tfid ~ amt ~ rno =>
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
          List(TradeTaxFee(tfid, Money(amt))),
          na.map(Money(_))
        )
    }
  }

  def store(exe: Trade): M[Trade] =
    sessionPool.use { session =>
      session.prepare(insertTrade).use { cmd =>
        cmd.execute(exe).void.map(_ => exe)
      }
    }

  private def storeTradeAndTaxFees(t: Trade, session: Session[M]): M[Trade] = {
    session.prepareAndExecute(insertTrade, t) *>
      session
        .prepareAndExecute(insertTaxFees(t.refNo, t.taxFees.size), t.taxFees)
        .void
        .map(_ => t)
  }

  def store(executions: NonEmptyList[Trade]): M[Unit] =
    sessionPool.use { session =>
      session.prepare(insertTrades(executions.size)).use { cmd =>
        cmd.execute(executions.toList).void.map(_ => ())
      }
    }
}

private object TradeQueries {
  val buySell = enum(BuySell, Type("buysell"))
  val taxFeeId = enum(TaxFeeId, Type("taxfeeid"))

  val tradeTaxFeeDecoder =
    varchar ~ varchar ~ varchar ~ buySell ~ numeric ~ numeric ~ timestamp ~ timestamp.opt ~ numeric.opt ~ taxFeeId ~ numeric ~ varchar

  val taxFeeDecoder: Decoder[TradeTaxFee] =
    (varchar ~ taxFeeId ~ numeric).map {
      case rno ~ tid ~ amt => TradeTaxFee(tid, Money(amt))
    }

  val tradeEncoder: Encoder[Trade] =
    (varchar ~ varchar ~ varchar ~ varchar ~ buySell ~ numeric ~ numeric ~ timestamp ~ timestamp.opt ~ numeric.opt).values
      .contramap(
        (t: Trade) =>
          t.refNo.value ~ t.accountNo.value ~ t.isin.value ~ t.market.toString ~ t.buySell ~ t.unitPrice.value ~ t.quantity.value ~ t.tradeDate ~ t.valueDate ~ t.netAmount
            .map(_.value)
      )

  def taxFeeEncoder(refNo: TradeReferenceNo): Encoder[TradeTaxFee] =
    (varchar ~ taxFeeId ~ numeric).values
      .contramap((t: TradeTaxFee) => refNo.value ~ t.taxFeeId ~ t.amount.value)

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

  def insertTaxFee(tradeRefNo: TradeReferenceNo): Command[TradeTaxFee] =
    sql"INSERT INTO tradeTaxFees (tradeRefNo, taxFeeId, amount) VALUES ${taxFeeEncoder(tradeRefNo)}".command

  def insertTaxFees(
      tradeRefNo: TradeReferenceNo,
      n: Int
  ): Command[List[TradeTaxFee]] = {
    val es = taxFeeEncoder(tradeRefNo).list(n)
    sql"INSERT INTO tradeTaxFees (tradeRefNo, taxFeeId, amount) VALUES $es".command
  }

  def insertTrades(n: Int): Command[List[Trade]] = {
    val enc = tradeEncoder.list(n)
    sql"INSERT INTO trades VALUES $enc".command
  }

  val selectByAccountNoAndDate =
    sql"""
        SELECT t.accountNo, 
               t.isinCode, 
               t.market, 
               t.buySellFlag, 
               t.unitPrice, 
               t.quantity, 
               t.tradeDate, 
               t.valueDate, 
               t.netAmount,
               f.taxFeeId,
               f.amount,
               t.tradeRefNo
        FROM trades t, tradeTaxFees f
        WHERE t.accountNo = ${varchar.cimap[AccountNo]}
          AND t.tradeDate = $timestamp
          AND t.tradeRefNo = f.tradeRefNo
    """.query(tradeTaxFeeDecoder)
}

// Smart constructor
object TradeRepositoryInterpreter {
  def make[M[_]: Sync](
      sessionPool: Resource[M, Session[M]]
  ): M[TradeRepositoryInterpreter[M]] =
    Sync[M].delay(new TradeRepositoryInterpreter[M](sessionPool))
}
