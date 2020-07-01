package org.github.domain
package model

import java.time.LocalDateTime

import _root_.io.estatico.newtype.macros.newtype

import shapeless.{::, HNil}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric._
import eu.timepit.refined.collection._
import eu.timepit.refined.boolean._
import eu.timepit.refined.generic._
import eu.timepit.refined._

import enumeratum._
import enumeratum.EnumEntry._
import squants.market._

object instrument {
  sealed trait InstrumentType extends EnumEntry 

  object InstrumentType extends Enum[InstrumentType] {

    case object CCY extends InstrumentType
    case object Equity extends InstrumentType
    case object FixedIncome extends InstrumentType

    val values = findValues
  }

  @newtype case class ISINCode(value: String Refined AllOf[MaxSize[W.`16`.T] :: MinSize[W.`16`.T] :: HNil])
  @newtype case class InstrumentName(value: String Refined NonEmpty)
  @newtype case class LotSize(value: Int Refined Positive)

  type ZeroToOne = Not[Less[W.`0.0`.T]] And Not[Greater[W.`1.0`.T]]

  private[instrument] case class Instrument (
    isinCode: ISINCode,
    name: InstrumentName,
    instrumentType: InstrumentType,
    dateOfIssue: Option[LocalDateTime],                    // for non CCY
    dateOfMaturity: Option[LocalDateTime],                 // for Fixed Income
    lotSize: LotSize,
    unitPrice: Option[Money],                              // for Equity
    couponRate: Option[Money],                             // for Fixed Income
    couponFrequency: Option[BigDecimal Refined ZeroToOne]  // for Fixed Income
  )
}
