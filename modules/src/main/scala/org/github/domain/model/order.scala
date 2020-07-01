package org.github.domain
package model

import java.time.LocalDateTime
import enumeratum._
import enumeratum.EnumEntry._

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._

import _root_.io.estatico.newtype.macros.newtype
import squants.market._

import io.chrisdavenport.cormorant._
import io.chrisdavenport.cormorant.generic.semiauto._
import io.chrisdavenport.cormorant.parser._
import io.chrisdavenport.cormorant.implicits._
import cats.implicits._
import java.time.Instant

import instrument._
import account._

object order {
  sealed trait BuySell extends EnumEntry 

  object BuySell extends Enum[BuySell] {

    case object Buy extends BuySell
    case object Sell extends BuySell

    val values = findValues
  }

  case class LineItem(
    instrument: ISINCode,
    quantity: BigDecimal Refined Positive,
    buySell: BuySell,
    price: Option[Money] = None
  )

  @newtype case class OrderNo(value: String Refined NonEmpty)

  case class Order(
    no: OrderNo,
    date: LocalDateTime,
    accountNo: AccountNo,
    items: List[LineItem] Refined NonEmpty
  )

  case class FrontOfficeOrder(
    accountNo: String,
    date: Instant,
    isin: String,
    qty: BigDecimal,
    buySell: String,
    price: Option[BigDecimal]
  )

  /**
   * accountNo,date,isin,qty,buySell,priceOpt
   */
  object Order {
    implicit val lr: LabelledRead[FrontOfficeOrder] = deriveLabelledRead
    implicit val lw: LabelledWrite[FrontOfficeOrder] = deriveLabelledWrite

    def ruinDelims(str: String) = augmentString(str).flatMap {
      case '\n' => "\r\n"
      case c => c.toString
    }

    def fromFrontOffice(order: String) = {
      parseComplete(order)
        .leftWiden[Error]
        .flatMap(_.readLabelled[FrontOfficeOrder].sequence)
    }
  }
}
