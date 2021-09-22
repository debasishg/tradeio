package tradex.domain
package model

import java.time.LocalDateTime

import cats.data.ValidatedNec
import cats.syntax.all._

import squants.market._

import NewtypeRefinedOps._
import enumeratum._
import io.estatico.newtype.macros.newtype

import eu.timepit.refined.numeric._
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection._
import eu.timepit.refined.boolean.AllOf
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._

import derevo.cats._
import derevo.circe.magnolia._
import derevo.derive
import io.circe.refined._
import model.order.UnitPrice

import _root_.shapeless.::
import _root_.shapeless.HNil

object instrument {
  // instrument
  type ISINCodeString = String Refined AllOf[
    MaxSize[W.`12`.T] ::
      MinSize[W.`12`.T] ::
      MatchesRegex[W.`"([A-Z]{2})((?![A-Z]{10}\b)[A-Z0-9]{10})"`.T] ::
      HNil
  ]

  @derive(decoder, encoder, eqv, show)
  @newtype case class ISINCode(value: ISINCodeString)

  @derive(decoder, encoder, eqv, show)
  @newtype case class InstrumentName(value: NonEmptyString)

  type LotSizeType = Int Refined Positive
  @derive(decoder, encoder, eqv, show)
  @newtype case class LotSize(value: LotSizeType)

  @derive(decoder, encoder, eqv, show)
  sealed abstract class InstrumentType(override val entryName: String) extends EnumEntry

  @derive(decoder, encoder, eqv, show)
  object InstrumentType extends Enum[InstrumentType] {
    case object CCY         extends InstrumentType("ccy")
    case object Equity      extends InstrumentType("equity")
    case object FixedIncome extends InstrumentType("fixedincome")

    val values = findValues
  }

  @derive(decoder, encoder, eqv, show)
  final case class Instrument private (
      isinCode: ISINCode,
      name: InstrumentName,
      instrumentType: InstrumentType,
      dateOfIssue: Option[LocalDateTime],    // for non CCY
      dateOfMaturity: Option[LocalDateTime], // for Fixed Income
      lotSize: LotSize,
      unitPrice: Option[UnitPrice],       // for Equity
      couponRate: Option[Money],          // for Fixed Income
      couponFrequency: Option[BigDecimal] // for Fixed Income
  )

  object Instrument {
    private[model] def validateISINCode(
        isin: String
    ): ValidatedNec[String, ISINCode] = validate[ISINCode](isin)

    private[model] def validateInstrumentName(
        name: String
    ): ValidatedNec[String, InstrumentName] =
      validate[InstrumentName](name)

    private[model] def validateLotSize(
        size: Int
    ): ValidatedNec[String, LotSize] = validate[LotSize](size)

    private[domain] def instrument(
        isinCode: String,
        name: String,
        instrumentType: InstrumentType,
        dateOfIssue: Option[LocalDateTime],    // for non CCY
        dateOfMaturity: Option[LocalDateTime], // for Fixed Income
        lotSize: Option[Int],
        unitPrice: Option[UnitPrice],       // for Equity
        couponRate: Option[Money],          // for Fixed Income
        couponFrequency: Option[BigDecimal] // for Fixed Income
    ): ValidatedNec[String, Instrument] = {
      (
        validateISINCode(isinCode),
        validateInstrumentName(name),
        validateLotSize(lotSize.getOrElse(0))
      ).mapN { (isin, name, ls) =>
        Instrument(
          isin,
          name,
          instrumentType,
          dateOfIssue,
          dateOfMaturity,
          ls,
          unitPrice,
          couponRate,
          couponFrequency
        )
      }
    }
  }
}
