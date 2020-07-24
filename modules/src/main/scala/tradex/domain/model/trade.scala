package tradex.domain
package model

import java.time.LocalDateTime

import cats.implicits._

import enumeratum._

import squants.market._

import common._
import NewtypeRefinedOps._
import newtypes._
import account._
import instrument._
import order._
import execution._
import market._
import enums._
import java.{util => ju}

object trade {
  sealed abstract class TaxFeeId(override val entryName: String)
      extends EnumEntry

  object TaxFeeId extends Enum[TaxFeeId] {
    case object TradeTax extends TaxFeeId("TradeTax")
    case object Commission extends TaxFeeId("Commission")
    case object VAT extends TaxFeeId("VAT")
    case object Surcharge extends TaxFeeId("Surcharge")

    val values = findValues
  }

  import TaxFeeId._

  // rates of tax/fees expressed as fractions of the principal of the trade
  final val rates: Map[TaxFeeId, BigDecimal] =
    Map(TradeTax -> 0.2, Commission -> 0.15, VAT -> 0.1)

  // tax and fees applicable for each market
  // Other signifies the general rule
  final val taxFeeForMarket: Map[Market, List[TaxFeeId]] =
    Map(
      Market.Other -> List(TradeTax, Commission),
      Market.Singapore -> List(TradeTax, Commission, VAT)
    )

  // get the list of tax/fees applicable for this trade
  // depends on the market
  final val forTrade: Trade => Option[List[TaxFeeId]] = { trade =>
    taxFeeForMarket.get(trade.market).orElse(taxFeeForMarket.get(Market.Other))
  }

  final def principal(trade: Trade): Money =
    Money(trade.unitPrice.value.value * trade.quantity.value.value)

  // combinator to value a tax/fee for a specific trade
  private def valueAs(trade: Trade, taxFeeId: TaxFeeId): Money = {
    ((rates get taxFeeId) map (_ * principal(trade))) getOrElse (Money(0))
  }

  // all tax/fees for a specific trade
  private def taxFeeCalculate(
      trade: Trade,
      taxFeeIds: List[TaxFeeId]
  ): List[TradeTaxFee] = {
    taxFeeIds
      .zip(taxFeeIds.map(valueAs(trade, _)))
      .map { case (tid, amt) => TradeTaxFee(tid, amt) }
  }

  private def netAmount(
      trade: Trade,
      taxFeeAmounts: List[TradeTaxFee]
  ): Money = {
    principal(trade) + taxFeeAmounts.map(_.amount).foldLeft(Money(0))(_ + _)
  }

  private[domain] final case class Trade private (
      accountNo: AccountNo,
      isin: ISINCode,
      refNo: TradeReferenceNo,
      market: Market,
      buySell: BuySell,
      unitPrice: UnitPrice,
      quantity: Quantity,
      tradeDate: LocalDateTime = today,
      valueDate: Option[LocalDateTime] = None,
      taxFees: List[TradeTaxFee] = List.empty,
      netAmount: Option[Money] = None
  )

  private[domain] final case class TradeTaxFee(
      taxFeeId: TaxFeeId,
      amount: Money
  )

  object Trade {
    def trade(
        accountNo: String,
        isin: String,
        refNo: String,
        market: String,
        buySell: String,
        unitPrice: BigDecimal,
        quantity: BigDecimal,
        td: LocalDateTime = today,
        vd: Option[LocalDateTime] = None,
        taxFees: List[TradeTaxFee] = List.empty,
        netAmt: Option[Money] = None
    ): ErrorOr[Trade] = {
      (
        validateTradeRefNo(refNo),
        Account.validateAccountNo(accountNo),
        Instrument.validateISINCode(isin),
        Order.validateQuantity(quantity),
        Order.validateUnitPrice(unitPrice),
        Order.validateBuySell(buySell),
        Execution.validateMarket(market)
      ).mapN { (ref, a, i, q, u, bs, m) =>
        val trd = Trade(a, i, ref, m, BuySell.withName(bs), u, q, td, vd)
        if (taxFees.isEmpty && !netAmt.isDefined) {
          val taxFees =
            forTrade(trd).map(taxFeeCalculate(trd, _)).getOrElse(List.empty)
          val netAmt = netAmount(trd, taxFees)
          trd.copy(taxFees = taxFees, netAmount = Option(netAmt))
        } else trd
      }.toEither
    }

    private[model] def validateTradeRefNo(
        refNo: String
    ): ValidationResult[TradeReferenceNo] = {
      validate[TradeReferenceNo](refNo).toValidated
    }

    def generateTradeReferenceNo(): TradeReferenceNo =
      validateTradeRefNo(ju.UUID.randomUUID().toString)
        .fold(
          errs =>
            throw new Exception(
              s"Unable to generate reference no : ${errs.toString}"
            ),
          identity
        )
  }
}
