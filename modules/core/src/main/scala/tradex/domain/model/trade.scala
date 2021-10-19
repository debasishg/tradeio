package tradex.domain
package model

import java.time.LocalDateTime
import java.util.UUID

import cats.Functor
import cats.data.NonEmptyList
import cats.syntax.all._

import enumeratum._

import squants.market._

import account._
import instrument._
import order._
import market._
import user.UserId
import io.estatico.newtype.macros.newtype
import derevo.cats._
import derevo.circe.magnolia._
import derevo.derive
import optics.uuid
import effects.GenUUID

object trade {
  @derive(decoder, encoder, eqv, show, uuid)
  @newtype
  @newtype case class TradeReferenceNo(value: UUID)

  @derive(decoder, encoder, eqv, show)
  sealed abstract class TaxFeeId(override val entryName: String) extends EnumEntry

  object TaxFeeId extends Enum[TaxFeeId] {
    case object TradeTax   extends TaxFeeId("TradeTax")
    case object Commission extends TaxFeeId("Commission")
    case object VAT        extends TaxFeeId("VAT")
    case object Surcharge  extends TaxFeeId("Surcharge")

    val values = findValues
  }

  @derive(decoder, encoder, eqv, show)
  final case class GenerateTradeFrontOfficeInput(
      frontOfficeOrders: NonEmptyList[FrontOfficeOrder],
      market: Market,
      brokerAccountNo: AccountNo,
      clientAccountNos: NonEmptyList[AccountNo]
  )

  import TaxFeeId._

  // rates of tax/fees expressed as fractions of the principal of the trade
  final val rates: Map[TaxFeeId, BigDecimal] =
    Map(TradeTax -> 0.2, Commission -> 0.15, VAT -> 0.1)

  // tax and fees applicable for each market
  // Other signifies the general rule applicable for all markets
  final val taxFeeForMarket: Map[Market, List[TaxFeeId]] =
    Map(
      Market.Other     -> List(TradeTax, Commission),
      Market.Singapore -> List(TradeTax, Commission, VAT)
    )

  // get the list of tax/fees applicable for this trade
  // depending on the market
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

  @derive(decoder, encoder, eqv, show)
  final case class Trade private (
      accountNo: AccountNo,
      isin: ISINCode,
      refNo: TradeReferenceNo,
      market: Market,
      buySell: BuySell,
      unitPrice: UnitPrice,
      quantity: Quantity,
      tradeDate: LocalDateTime = today,
      valueDate: Option[LocalDateTime] = None,
      userId: Option[UserId] = None,
      taxFees: List[TradeTaxFee] = List.empty,
      netAmount: Option[Money] = None
  )

  @derive(decoder, encoder, eqv, show)
  private[domain] final case class TradeTaxFee(
      taxFeeId: TaxFeeId,
      amount: Money
  )

  object Trade {
    def trade[F[_]: Functor: GenUUID](
        accountNo: AccountNo,
        isin: ISINCode,
        market: Market,
        buySell: BuySell,
        unitPrice: UnitPrice,
        quantity: Quantity,
        tradeDate: LocalDateTime = today,
        valueDate: Option[LocalDateTime] = None,
        userId: Option[UserId] = None
    ): F[Trade] = {
      ID.make[F, TradeReferenceNo]
        .map { refNo =>
          Trade(
            accountNo,
            isin,
            refNo,
            market,
            buySell,
            unitPrice,
            quantity,
            tradeDate,
            valueDate,
            userId
          )
        }
    }

    def withTaxFee(trade: Trade): Trade = {
      if (trade.taxFees.isEmpty && !trade.netAmount.isDefined) {
        val taxFees =
          forTrade(trade).map(taxFeeCalculate(trade, _)).getOrElse(List.empty)
        val netAmt = netAmount(trade, taxFees)
        trade.copy(taxFees = taxFees, netAmount = Option(netAmt))
      } else trade
    }
  }
}
