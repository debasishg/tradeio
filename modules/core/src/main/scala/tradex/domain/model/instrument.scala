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
import tradex.domain.model.instrument.InstrumentType.CCY
import tradex.domain.model.instrument.InstrumentType.Equity
import tradex.domain.model.instrument.InstrumentType.FixedIncome

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

  @derive(decoder, encoder, show)
  final case class CreateInstrument private (
      isinCode: String,
      name: String,
      instrumentType: InstrumentType,
      dateOfIssue: Option[LocalDateTime],    // for non CCY
      dateOfMaturity: Option[LocalDateTime], // for Fixed Income
      lotSize: Int,
      unitPrice: Option[BigDecimal],      // for Equity
      couponRate: Option[Money],          // for Fixed Income
      couponFrequency: Option[BigDecimal] // for Fixed Income
  ) {
    def toDomain: ValidatedNec[String, Instrument] = {
      Instrument.instrument(
        isinCode,
        name,
        instrumentType,
        dateOfIssue,
        dateOfMaturity,
        Some(lotSize),
        unitPrice,
        couponRate,
        couponFrequency
      )
    }
  }

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

    private[model] def validateUnitPrice(
        price: BigDecimal
    ): ValidatedNec[String, UnitPrice] = {
      validate[UnitPrice](price)
        .leftMap(_ :+ s"Unit Price has to be positive: found $price")
    }

    private[domain] def instrument(
        isinCode: String,
        name: String,
        instrumentType: InstrumentType,
        dateOfIssue: Option[LocalDateTime],    // for non CCY
        dateOfMaturity: Option[LocalDateTime], // for Fixed Income
        lotSize: Option[Int],
        unitPrice: Option[BigDecimal],      // for Equity
        couponRate: Option[Money],          // for Fixed Income
        couponFrequency: Option[BigDecimal] // for Fixed Income
    ): ValidatedNec[String, Instrument] = instrumentType match {
      case CCY         => ccy(isinCode, name, dateOfIssue)
      case Equity      => equity(isinCode, name, dateOfIssue, lotSize, unitPrice)
      case FixedIncome => fixedIncome(isinCode, name, dateOfIssue, dateOfMaturity, lotSize, couponRate, couponFrequency)
    }

    private[domain] def ccy(
        isinCode: String,
        name: String,
        dateOfIssue: Option[LocalDateTime] // for non CCY
    ): ValidatedNec[String, Instrument] = {
      (
        validateISINCode(isinCode),
        validateInstrumentName(name)
      ).mapN { (isin, name) =>
        Instrument(
          isin,
          name,
          InstrumentType.CCY,
          dateOfIssue,
          None,
          LotSize(1),
          None,
          None,
          None
        )
      }
    }

    private[domain] def fixedIncome(
        isinCode: String,
        name: String,
        dateOfIssue: Option[LocalDateTime],    // for non CCY
        dateOfMaturity: Option[LocalDateTime], // for Fixed Income
        lotSize: Option[Int],
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
          InstrumentType.FixedIncome,
          dateOfIssue,
          dateOfMaturity,
          ls,
          None,
          couponRate,
          couponFrequency
        )
      }
    }

    private[domain] def equity(
        isinCode: String,
        name: String,
        dateOfIssue: Option[LocalDateTime], // for non CCY
        lotSize: Option[Int],
        unitPrice: Option[BigDecimal] // for Equity
    ): ValidatedNec[String, Instrument] = {
      (
        validateISINCode(isinCode),
        validateInstrumentName(name),
        validateLotSize(lotSize.getOrElse(0)),
        validateUnitPrice(unitPrice.getOrElse(ZERO_BIG_DECIMAL))
      ).mapN { (isin, name, ls, uprice) =>
        Instrument(
          isin,
          name,
          InstrumentType.Equity,
          dateOfIssue,
          None,
          ls,
          Some(uprice),
          None,
          None
        )
      }
    }
  }
}
