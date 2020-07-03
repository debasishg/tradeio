package org.github.domain
package model

import java.time.LocalDateTime

import cats.implicits._

import enumeratum._
import enumeratum.EnumEntry._

import squants.market._

import common._
import newtypes._
import account._
import instrument._
import execution._
import market._

object trade {

  sealed abstract class TaxFeeId(override val entryName: String) extends EnumEntry

  object TaxFeeId extends Enum[TaxFeeId] {
    case object TradeTax extends TaxFeeId("TradeTax")
    case object Commission extends TaxFeeId("Commission")
    case object VAT extends TaxFeeId("VAT")
    case object Surcharge extends TaxFeeId("Surcharge")

    val values = findValues
  }

  import TaxFeeId._

  // rates of tax/fees expressed as fractions of the principal of the trade
  final val rates: Map[TaxFeeId, BigDecimal] = Map(TradeTax -> 0.2, Commission -> 0.15, VAT -> 0.1)

  // tax and fees applicable for each market
  // Other signifies the general rule
  final val taxFeeForMarket: Map[Market, List[TaxFeeId]] = 
    Map(Market.Other -> List(TradeTax, Commission), Market.Singapore -> List(TradeTax, Commission, VAT))

  // get the list of tax/fees applicable for this trade
  // depends on the market
  final val forTrade: Trade => Option[List[TaxFeeId]] = {trade =>
    taxFeeForMarket.get(trade.market).orElse(taxFeeForMarket.get(Market.Other)) 
  }

  final def principal(trade: Trade)(implicit ctx: MoneyContext): Money = Money(trade.unitPrice.value * trade.quantity.value)

  // combinator to value a tax/fee for a specific trade
  private def valueAs(trade: Trade, taxFeeId: TaxFeeId)
    (implicit ctx: MoneyContext): Money = {
    ((rates get taxFeeId) map (_ * principal(trade))) getOrElse (Money(0)) 
  }

  // all tax/fees for a specific trade
  private def taxFeeCalculate(trade: Trade, taxFeeIds: List[TaxFeeId])
    (implicit ctx: MoneyContext): List[(TaxFeeId, Money)] = {
    taxFeeIds zip (taxFeeIds.map(valueAs(trade, _)))
  }

  private [domain] final case class Trade (
    accountNo: AccountNo, 
    isin: ISINCode, 
    refNo: TradeReferenceNo, 
    market: Market,
    unitPrice: UnitPrice, 
    quantity: Quantity, 
    tradeDate: LocalDateTime = today,
    valueDate: Option[LocalDateTime] = None, 
    taxFees: Option[List[(TaxFeeId, Money)]] = None, 
    netAmount: Option[Money] = None
  )

  object Trade {
    def trade(
      accountNo: AccountNo, 
      isin: ISINCode, 
      refNo: TradeReferenceNo, 
      market: Market, 
      unitPrice: UnitPrice, 
      quantity: Quantity,
      td: LocalDateTime = today, 
      vd: Option[LocalDateTime] = None): ErrorOr[Trade] = {
        (

          Account.validateAccountNo(accountNo),
          Instrument.validateISINCode(isin),
          Execution.validateQuantity(quantity),
          Execution.validateUnitPrice(unitPrice)

        ).mapN {(a, i, q, u) =>
          Trade(a, i, refNo, market, u, q, td, vd)
        }.toEither
      }
  }
}
