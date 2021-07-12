package tradex.domain
package repository
package interpreter.skunk

import java.time.{LocalDate, LocalDateTime}

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

final class TradeRepositoryInterpreter[M[_]: Concurrent] private (
    sessionPool: Resource[M, Session[M]]
) extends TradeRepository[M] {
  import TradeQueries._

  // semigroup that combines trades with same reference number
  // used in combining join records between trades and taxFees tables
  // NOT a generic semigroup that combines all trades - only specific
  // to this query - hence not added in the companion object
  implicit val tradeConcatSemigroup: Semigroup[Trade] = new Semigroup[Trade] {
    def combine(x: Trade, y: Trade): Trade =
      x.copy(taxFees = x.taxFees ++ y.taxFees)
  }

  def query(accountNo: String, date: LocalDate): M[List[Trade]] =
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
                  makeSingleTradeTaxFees(refNo, lis)
                singleTradeTaxFeeLine.tail.foldLeft(singleTradeTaxFeeLine.head)(
                  Semigroup[Trade].combine
                )
            }.toList
          }
      }
    }

  private def makeSingleTradeTaxFees(
      trdRefNo: String,
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
        Trade
          .trade(
            ano,
            isin,
            rno,
            mkt,
            bs.entryName,
            up,
            qty,
            td,
            vd,
            List(TradeTaxFee(tfid, Money(amt))),
            na.map(Money(_))
          )
          .fold(errs => throw new Exception(errs.toString), identity)
    }
  }

  def store(exe: Trade): M[Trade] =
    sessionPool.use { session =>
      session.prepare(insertTrade).use {
        _.execute(exe).void.map(_ => exe)
      }
    }

  // FIXME: Need to make transactional: Refer to Page 177 of pfp-scala
  private def storeTradeAndTaxFees(t: Trade, session: Session[M]): M[Trade] = {
    val taxFees = t.taxFees
    session.prepare(insertTrade).use(_.execute(t)) *>
      session
        .prepare(insertTaxFees(t.refNo, taxFees))
        .use { cmd =>
          cmd.execute(taxFees)
        }
        .void
        .map(_ => t)
  }

  def store(trades: NonEmptyList[Trade]): M[Unit] =
    sessionPool.use { session =>
      val ts = trades.toList
      session.prepare(insertTrades(ts)).use(_.execute(ts)).void.map(_ => ())
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
          t.refNo.value.value ~ t.accountNo.value.value ~ t.isin.value.value ~ t.market.toString ~ t.buySell ~ t.unitPrice.value.value ~ t.quantity.value.value ~ t.tradeDate ~ t.valueDate ~ t.netAmount
            .map(_.value)
      )

  def taxFeeEncoder(refNo: TradeReferenceNo): Encoder[TradeTaxFee] =
    (varchar ~ taxFeeId ~ numeric).values
      .contramap(
        (t: TradeTaxFee) => refNo.value.value ~ t.taxFeeId ~ t.amount.value
      )

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
      taxFees: List[TradeTaxFee]
  ): Command[taxFees.type] = {
    val es = taxFeeEncoder(tradeRefNo).values.list(taxFees)
    sql"INSERT INTO tradeTaxFees (tradeRefNo, taxFeeId, amount) VALUES $es".command
  }

//   def insertTrades(n: Int): Command[List[Trade]] = {
//     val enc = tradeEncoder.list(n)
//     sql"INSERT INTO trades VALUES $enc".command
//   }

  def insertTrades(trades: List[Trade]): Command[trades.type] = {
    val enc = tradeEncoder.values.list(trades)
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
        WHERE t.accountNo = $varchar
          AND DATE(t.tradeDate) = $date
          AND t.tradeRefNo = f.tradeRefNo
    """.query(tradeTaxFeeDecoder)
}

// Smart constructor
object TradeRepositoryInterpreter {
  def make[M[_]: Concurrent](
      sessionPool: Resource[M, Session[M]]
  ): TradeRepositoryInterpreter[M] =
    new TradeRepositoryInterpreter[M](sessionPool)
}
