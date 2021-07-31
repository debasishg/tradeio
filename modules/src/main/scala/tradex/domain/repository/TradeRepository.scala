package tradex.domain
package repository

import java.time.{LocalDate, LocalDateTime}

import cats.Semigroup
import cats.data.NonEmptyList
import cats.syntax.all._
import cats.effect._

import skunk._
import skunk.data.Type
import skunk.codec.all._
import skunk.implicits._

import squants.market._
import org.typelevel.log4cats.Logger

import model.market._
import model.trade._
import model.order.BuySell

trait TradeRepository[F[_]] {
  /** query by account number and trade date (compares using the date part only) */
  def query(accountNo: String, date: LocalDate): F[List[Trade]]

  /** query by market */
  def queryByMarket(market: Market): F[List[Trade]]

  /** query all trades */
  def all: F[List[Trade]]

  /** store */
  def store(trd: Trade): F[Trade]

  /** store many trades */
  def store(trades: NonEmptyList[Trade]): F[Unit]
}

object TradeRepository {
  def make[F[_]: Concurrent: Logger](
      postgres: Resource[F, Session[F]]
  ): TradeRepository[F] =
    new TradeRepository[F] {
      import TradeRepositorySQL._

      // semigroup that combines trades with same reference number
      // used in combining join records between trades and taxFees tables
      // NOT a generic semigroup that combines all trades - only specific
      // to this query - hence not added in the companion object
      implicit val tradeConcatSemigroup: Semigroup[Trade] =
        new Semigroup[Trade] {
          def combine(x: Trade, y: Trade): Trade =
            x.copy(taxFees = x.taxFees ++ y.taxFees)
        }

      def query(accountNo: String, date: LocalDate): F[List[Trade]] =
        postgres.use { session =>
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
                    singleTradeTaxFeeLine.tail
                      .foldLeft(singleTradeTaxFeeLine.head)(
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
              String ~
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
                List(TradeTaxFee(TaxFeeId.withName(tfid), Money(amt))),
                na.map(Money(_))
              )
              .fold(errs => throw new Exception(errs.toString), identity)
        }
      }

      def queryByMarket(market: Market): F[List[Trade]] =
        postgres.use { session =>
          session.prepare(selectByMarket).use { ps =>
            ps.stream(market.entryName, 1024)
              .compile
              .toList
              .map(_.groupBy(_._2))
              .map { m =>
                m.map {
                  case (refNo, lis) =>
                    val singleTradeTaxFeeLine =
                      makeSingleTradeTaxFees(refNo, lis)
                    singleTradeTaxFeeLine.tail
                      .foldLeft(singleTradeTaxFeeLine.head)(
                        Semigroup[Trade].combine
                      )
                }.toList
              }
          }
        }

      def all: F[List[Trade]] =
        postgres.use { session =>
          session.prepare(selectAll).use { ps =>
            ps.stream(skunk.Void, 1024)
              .compile
              .toList
              .map(_.groupBy(_._2))
              .map { m =>
                m.map {
                  case (refNo, lis) =>
                    val singleTradeTaxFeeLine =
                      makeSingleTradeTaxFees(refNo, lis)
                    singleTradeTaxFeeLine.tail
                      .foldLeft(singleTradeTaxFeeLine.head)(
                        Semigroup[Trade].combine
                      )
                }.toList
              }
          }
        }

      def store(trade: Trade): F[Trade] =
        postgres.use { session =>
          storeTradeAndTaxFees(trade, session)
        }

      private def storeTradeAndTaxFees(
          t: Trade,
          session: Session[F]
      ): F[Trade] = {
        val r = for {
          p1 <- session.prepare(insertTrade)
          p2 <- session.prepare(insertTaxFees(t.refNo, t.taxFees))
        } yield (p1, p2)
        r.use {
            case (p1, p2) =>
              session.transaction.use { xa =>
                for {
                  _ <- p1.execute(t)
                  _ <- p2.execute(t.taxFees)
                } yield ()
              }
          }
          .map(_ => t)
      }

      def store(trades: NonEmptyList[Trade]): F[Unit] =
        postgres.use { session =>
          trades.toList
            .traverse_(trade => storeTradeAndTaxFees(trade, session))
        }
    }
}

private object TradeRepositorySQL {
  val buySell = enum(BuySell, Type("buysell"))
  val taxFeeId = enum(TaxFeeId, Type("taxfeeid"))

  val tradeTaxFeeDecoder =
    varchar ~ varchar ~ varchar ~ buySell ~ numeric ~ numeric ~ timestamp ~ timestamp.opt ~ numeric.opt ~ varchar ~ numeric ~ varchar

  val taxFeeDecoder: Decoder[TradeTaxFee] =
    (varchar ~ varchar ~ numeric).map {
      case rno ~ tid ~ amt => TradeTaxFee(TaxFeeId.withName(tid), Money(amt))
    }

  val tradeEncoder: Encoder[Trade] =
    (varchar ~ varchar ~ varchar ~ varchar ~ buySell ~ numeric ~ numeric ~ timestamp ~ timestamp.opt ~ numeric.opt).values
      .contramap(
        (t: Trade) =>
          t.refNo.value.value ~ t.accountNo.value.value ~ t.isin.value.value ~ t.market.entryName ~ t.buySell ~ t.unitPrice.value.value ~ t.quantity.value.value ~ t.tradeDate ~ t.valueDate ~ t.netAmount
            .map(_.value)
      )

  def taxFeeEncoder(refNo: TradeReferenceNo): Encoder[TradeTaxFee] =
    (varchar ~ varchar ~ numeric).values
      .contramap(
        (t: TradeTaxFee) =>
          refNo.value.value ~ t.taxFeeId.entryName ~ t.amount.value
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
    val es = taxFeeEncoder(tradeRefNo).list(taxFees)
    sql"INSERT INTO tradeTaxFees (tradeRefNo, taxFeeId, amount) VALUES $es".command
  }

  def insertTrades(trades: List[Trade]): Command[trades.type] = {
    val enc = tradeEncoder.list(trades)
    sql"""
        INSERT INTO trades
        (
          tradeRefNo
          , accountNo
          , isinCode
          , market
          , buySellFlag
          , unitPrice
          , quantity
          , tradeDate
          , valueDate
          , netAmount
        )
        VALUES $enc""".command
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

  val selectByMarket =
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
        WHERE t.market = $varchar
          AND t.tradeRefNo = f.tradeRefNo
    """.query(tradeTaxFeeDecoder)

  val selectAll =
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
        WHERE t.tradeRefNo = f.tradeRefNo
    """.query(tradeTaxFeeDecoder)
}
