package tradex.domain
package model

import java.time.LocalDateTime
import java.time.Instant
import java.util.UUID

import cats.effect._
import cats.data.NonEmptyList
import cats.implicits._

import instrument._
import account._
import common._
import newtypes._
import enums._

object order {
  private[domain] final case class LineItem(
      instrument: ISINCode,
      quantity: Quantity,
      unitPrice: UnitPrice,
      buySell: BuySell
  )

  private[domain] final case class Order(
      no: OrderNo,
      date: LocalDateTime,
      accountNo: AccountNo,
      items: NonEmptyList[LineItem]
  )

  private[domain] final case class FrontOfficeOrder(
      accountNo: String,
      date: Instant,
      isin: String,
      qty: BigDecimal,
      unitPrice: BigDecimal,
      buySell: String
  )

  object Order {
    /**
      * Domain validation for `FrontOfficeOrder` is done here. Creates
      * records after validation
      */
    private[domain] def create(
        frontOfficeOrders: List[FrontOfficeOrder]
    ): ErrorOr[List[Order]] = {
      frontOfficeOrders
        .traverse(validateFrontOfficeOrder)
        .fold(
          Left(_),
          fos => Right(createImpl(fos))
        )
    }

    /**
      * Creates `Order` from `FrontOfficeOrder`
      */
    private def createImpl(foOrders: List[FrontOfficeOrder]): List[Order] = {
      foOrders
        .groupBy(_.accountNo)
        .map { orderForAccount =>
          val ano = orderForAccount._1
          val fords = orderForAccount._2
          val lineItems = fords.map { ford =>
            LineItem(
              ISINCode(ford.isin),
              Quantity(ford.qty),
              UnitPrice(ford.unitPrice),
              BuySell.withName(ford.buySell)
            )
          }
          Order(
            OrderNo(UUID.randomUUID().toString()),
            today,
            AccountNo(ano),
            NonEmptyList.of(lineItems.head, lineItems.tail: _*)
          )
        }
        .toList
    }

    /**
      * Validates a single `FrontOfficeOrder` record (applicative style)
      */
    private def validateFrontOfficeOrder(
        fo: FrontOfficeOrder
    ): ErrorOr[FrontOfficeOrder] = {
      (
        Account.validateAccountNo(AccountNo(fo.accountNo)),
        Instrument.validateISINCode(ISINCode(fo.isin)),
        validateQuantity(Quantity(fo.qty)),
        validateUnitPrice(UnitPrice(fo.unitPrice)),
        validateBuySell(fo.buySell)
      ).mapN { (ano, ins, q, p, bs) =>
        FrontOfficeOrder(ano.value, fo.date, ins.value, q, p, bs)
      }.toEither
    }

    private[model] def validateQuantity(
        qty: Quantity
    ): ValidationResult[BigDecimal] =
      (if (qty.value <= 0)
         s"Quantity has to be positive: found $qty".invalidNec
       else qty.validNec).map(_.value)

    private[model] def validateUnitPrice(
        price: UnitPrice
    ): ValidationResult[BigDecimal] =
      (if (price.value <= 0)
         s"Unit Price has to be positive: found $price".invalidNec
       else price.validNec).map(_.value)

    private[model] def validateBuySell(bs: String): ValidationResult[String] = {
      BuySell
        .withNameEither(bs)
        .toValidatedNec
        .map(_.entryName)
        .leftMap(_.map(_.toString))
    }

//     def main(): Unit = {
//       val o1 =
//         FrontOfficeOrder("a-1", Instant.now(), "isin-12345", 100.00, "B")
//       val o2 =
//         FrontOfficeOrder("a-1", Instant.now(), "isin-12346", 200.00, "S")
//       val o3 =
//         FrontOfficeOrder("a-2", Instant.now(), "isin-12345", 100.00, "B")
//       val orders = List(o1, o2, o3)
//
//       val csv = orders.writeComplete.print(Printer.default)
//       println(csv)
//       // accountNo,date,isin,qty,buySell
//       // a-1,2020-07-02T05:05:13.619Z,isin-12345,100.0,B
//       // a-1,2020-07-02T05:05:13.619Z,isin-12346,200.0,S
//       // a-2,2020-07-02T05:05:13.619Z,isin-12345,100.0,B
//
//       fromFrontOffice(csv) match {
//         case Left(e) => println(e)
//         case Right(v) => v.foreach(println)
//       }
//     }
  }
}
