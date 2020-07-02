package org.github.domain
package model

import java.time.LocalDateTime

import cats._
import cats.data._
import cats.implicits._
import cats.instances.all._

import _root_.io.estatico.newtype.macros.newtype

import enumeratum._
import enumeratum.EnumEntry._
import squants.market._

import common._

object instrument {
  sealed trait InstrumentType extends EnumEntry

  object InstrumentType extends Enum[InstrumentType] {
    case object CCY extends InstrumentType
    case object Equity extends InstrumentType
    case object FixedIncome extends InstrumentType

    val values = findValues
  }

  @newtype case class ISINCode(value: String)
  @newtype case class InstrumentName(value: String)
  @newtype case class LotSize(value: Int)

  case class Instrument(
      isinCode: ISINCode,
      name: InstrumentName,
      instrumentType: InstrumentType,
      dateOfIssue: Option[LocalDateTime], // for non CCY
      dateOfMaturity: Option[LocalDateTime], // for Fixed Income
      lotSize: LotSize,
      unitPrice: Option[Money], // for Equity
      couponRate: Option[Money], // for Fixed Income
      couponFrequency: Option[BigDecimal] // for Fixed Income
  )

  object Instrument {
    private[model] def validateISINCode(isin: ISINCode): ValidationResult[ISINCode] =
      if (isin.value.isEmpty || isin.value.size != 12)
        s"ISIN code has to be 12 characters long: found $isin".invalidNec
      else isin.validNec
  }

}
