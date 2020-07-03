package org.github.domain
package model

import java.time.LocalDateTime
import java.io.InputStream
import java.time.Instant
import java.util.UUID

import enumeratum._
import enumeratum.EnumEntry._

import cats.effect._
import cats.data.NonEmptyList
import cats.implicits._

import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._

import squants.market._

import io.chrisdavenport.cormorant._
import io.chrisdavenport.cormorant.generic.semiauto._
import io.chrisdavenport.cormorant.parser._
import io.chrisdavenport.cormorant.implicits._

import instrument._
import account._
import common._
import newtypes._

object order {
  sealed abstract class BuySell(override val entryName: String) extends EnumEntry

  object BuySell extends Enum[BuySell] {
    case object Buy extends BuySell("B")
    case object Sell extends BuySell("S")

    val values = findValues
  }

  private[model] final case class LineItem(
      instrument: ISINCode,
      quantity: Quantity,
      buySell: BuySell
  )

  private[model] final case class Order(
      no: OrderNo,
      date: LocalDateTime,
      accountNo: AccountNo,
      items: NonEmptyList[LineItem] 
  )

  private[model] final case class FrontOfficeOrder(
      accountNo: String,
      date: Instant,
      isin: String,
      qty: BigDecimal,
      buySell: String
  )

  object Order {
    implicit val lr: LabelledRead[FrontOfficeOrder] = deriveLabelledRead
    implicit val lw: LabelledWrite[FrontOfficeOrder] = deriveLabelledWrite

    /**
     * Create orders reading an input stream containing csv data from
     * front office.
     * The format is as follows:
     * accountNo,date,isin,qty,buySell
     */ 
    def createOrders(in: InputStream): IO[ErrorOr[List[Order]]] = {
      val acquire = IO {
        scala.io.Source.fromInputStream(in)
      }

      Resource
        .fromAutoCloseable(acquire)
        .use(source => IO(createOrders(source.mkString)))
    }

    /**
     * Create orders reading a string containing newline separated csv data from
     * front office.
     * The format is as follows:
     * accountNo,date,isin,qty,buySell
     */ 
    def createOrders(frontOfficeCsv: String): ErrorOr[List[Order]] = 
      fromFrontOffice(frontOfficeCsv).flatMap(create)

    /**
     * Workhorse method that parses csv data and creates `FrontOfficeOrder`.
     * No domain validation is done here
     */ 
    private def fromFrontOffice(order: String): ErrorOr[List[FrontOfficeOrder]] = {
      parseComplete(order)
        .leftWiden[Error]
        .flatMap(_.readLabelled[FrontOfficeOrder].sequence)
        .toValidatedNec
        .toEither
        .leftMap(_.map(_.toString))
    }

    /**
     * Domain validation for `FrontOfficeOrder` is done here. Creates
     * records after validation
     */ 
    private def create(frontOfficeOrders: List[FrontOfficeOrder]): ErrorOr[List[Order]] = {
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
      foOrders.groupBy(_.accountNo).map { orderForAccount =>
        val ano = orderForAccount._1
        val fords = orderForAccount._2
        val lineItems = fords.map { ford =>
          LineItem(ISINCode(ford.isin), Quantity(ford.qty), BuySell.withName(ford.buySell))
        }
        Order(
          OrderNo(UUID.randomUUID().toString()), 
          today, 
          AccountNo(ano), 
          NonEmptyList.of(lineItems.head, lineItems.tail:_*)
        )
      }.toList
    }

    /**
     * Validates a single `FrontOfficeOrder` record (applicative style)
     */ 
    private def validateFrontOfficeOrder(fo: FrontOfficeOrder): ErrorOr[FrontOfficeOrder] = {
      (

        Account.validateAccountNo(AccountNo(fo.accountNo)),
        Instrument.validateISINCode(ISINCode(fo.isin)),
        validateQuantity(Quantity(fo.qty)),
        validateBuySell(fo.buySell)

      ).mapN { (ano, ins, q, bs) =>
        FrontOfficeOrder(ano.value, fo.date, ins.value, q, bs)
      }.toEither
    }

    private[model] def validateQuantity(qty: Quantity): ValidationResult[BigDecimal] =
      (if (qty.value <= 0)
        s"Quantity has to be positive: found $qty".invalidNec
      else qty.validNec).map(_.value)

    private[model] def validateBuySell(bs: String): ValidationResult[String] = {
      BuySell
        .withNameEither(bs)
        .toValidatedNec
        .map(_.entryName)
        .leftMap(_.map(_.toString))
    }

    private def ruinDelims(str: String) = augmentString(str).flatMap {
      case '\n' => "\r\n"
      case c => c.toString
    }

    def main(): Unit = {
      val o1 =
        FrontOfficeOrder("a-1", Instant.now(), "isin-12345", 100.00, "B")
      val o2 =
        FrontOfficeOrder("a-1", Instant.now(), "isin-12346", 200.00, "S")
      val o3 =
        FrontOfficeOrder("a-2", Instant.now(), "isin-12345", 100.00, "B")
      val orders = List(o1, o2, o3)

      val csv = orders.writeComplete.print(Printer.default)
      println(csv)
      // accountNo,date,isin,qty,buySell
      // a-1,2020-07-02T05:05:13.619Z,isin-12345,100.0,B
      // a-1,2020-07-02T05:05:13.619Z,isin-12346,200.0,S
      // a-2,2020-07-02T05:05:13.619Z,isin-12345,100.0,B

      fromFrontOffice(csv) match {
        case Left(e) => println(e)
        case Right(v) => v.foreach(println)
      }
    }
  }
}
