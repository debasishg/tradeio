package tradex.domain
package model

import java.time.LocalDateTime
import java.time.Instant
import java.util.UUID

import cats.data.NonEmptyList
import cats.data.EitherNec
import cats.syntax.all._
import cats.instances.list._

import instrument._
import account._
import NewtypeRefinedOps._
import enumeratum._
import io.estatico.newtype.macros.newtype

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric._
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._

import derevo.cats._
import derevo.circe.magnolia._
import derevo.derive
import io.circe.refined._

object order {
  @derive(decoder, encoder, eqv, show)
  @newtype case class OrderNo(value: NonEmptyString)

  @derive(decoder, encoder, eqv, show)
  @newtype case class Quantity(value: BigDecimal Refined NonNegative)

  @derive(decoder, encoder, eqv, show)
  @newtype case class UnitPrice(value: BigDecimal Refined Positive)

  @derive(decoder, encoder, eqv, show)
  sealed abstract class BuySell(override val entryName: String) extends EnumEntry

  object BuySell extends Enum[BuySell] {
    case object Buy  extends BuySell("buy")
    case object Sell extends BuySell("sell")

    val values = findValues
  }

  // domain entity
  @derive(decoder, encoder, eqv, show)
  private[domain] final case class LineItem private (
      orderNo: OrderNo,
      instrument: ISINCode,
      quantity: Quantity,
      unitPrice: UnitPrice,
      buySell: BuySell
  )

  @derive(decoder, encoder, eqv, show)
  final case class Order private (
      no: OrderNo,
      date: LocalDateTime,
      accountNo: AccountNo,
      items: NonEmptyList[LineItem]
  )

  @derive(decoder, encoder, eqv, show)
  final case class FrontOfficeOrder private (
      accountNo: String,
      date: Instant,
      isin: String,
      qty: BigDecimal,
      unitPrice: BigDecimal,
      buySell: String
  )

  object Order {

    /** Domain validation for `FrontOfficeOrder` is done here. Creates
      * records after validation
      */
    private[domain] def create(
        frontOfficeOrders: NonEmptyList[FrontOfficeOrder]
    ): EitherNec[String, List[Order]] = {
      frontOfficeOrders.toList
        .groupBy(_.accountNo)
        .map { case (ano, forders) =>
          makeOrder(UUID.randomUUID.toString, today, ano, forders)
        }
        .toList
        .sequence
    }

    private[domain] def makeOrder(
        ono: String,
        odt: LocalDateTime,
        ano: String,
        forders: List[FrontOfficeOrder]
    ): EitherNec[String, Order] = {
      forders
        .map { fo =>
          makeLineItem(ono, fo.isin, fo.qty, fo.unitPrice, fo.buySell)
        }
        .sequence
        .map { items =>
          makeOrder(ono, odt, ano, NonEmptyList.of(items.head, items.tail: _*))
        }
        .fold(Left(_), identity)
    }

    private[domain] def makeLineItem(
        ono: String,
        isin: String,
        quantity: BigDecimal,
        unitPrice: BigDecimal,
        buySell: String
    ): EitherNec[String, LineItem] = {
      (
        validateOrderNo(ono),
        Instrument.validateISINCode(isin),
        validateQuantity(quantity),
        validateUnitPrice(unitPrice),
        validateBuySell(buySell)
      ).mapN { (orderNo, isin, qty, price, bs) =>
        LineItem(orderNo, isin, qty, price, BuySell.withName(bs))
      }
    }

    private[domain] def makeOrder(
        ono: String,
        orderDate: LocalDateTime,
        accountNo: String,
        lineItems: NonEmptyList[LineItem]
    ): EitherNec[String, Order] = {
      (
        validateOrderNo(ono),
        Account.validateAccountNo(accountNo)
      ).mapN { (orderNo, accountNo) =>
        Order(
          orderNo,
          orderDate,
          accountNo,
          lineItems
        )
      }
    }

    private[model] def validateQuantity(
        qty: BigDecimal
    ): EitherNec[String, Quantity] = {
      validate[Quantity](qty)
        .leftMap(_ :+ s"Quantity has to be positive: found $qty")
    }

    private[model] def validateUnitPrice(
        price: BigDecimal
    ): EitherNec[String, UnitPrice] = {
      validate[UnitPrice](price)
        .leftMap(_ :+ s"Unit Price has to be positive: found $price")
    }

    private[model] def validateOrderNo(
        orderNo: String
    ): EitherNec[String, OrderNo] = {
      validate[OrderNo](orderNo)
    }

    private[model] def validateBuySell(
        bs: String
    ): EitherNec[String, String] = {
      BuySell
        .withNameEither(bs)
        .toEitherNec
        .map(_.entryName)
        .leftMap(_.map(_.toString))
    }
  }
}
