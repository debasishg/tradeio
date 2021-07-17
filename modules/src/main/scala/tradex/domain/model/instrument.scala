package tradex.domain
package model

import java.time.LocalDateTime

import cats.data.EitherNec
import cats.syntax.all._

import squants.market._

import NewtypeRefinedOps._
import newtypes._
import enums._

object instrument {
  private[domain] final case class Instrument private (
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
    private[model] def validateISINCode(
        isin: String
    ): EitherNec[String, ISINCode] = validate[ISINCode](isin)

    private[model] def validateInstrumentName(
        name: String
    ): EitherNec[String, InstrumentName] =
      validate[InstrumentName](name)

    private[model] def validateLotSize(
        size: Short
    ): EitherNec[String, LotSize] = validate[LotSize](size)

    private[domain] def instrument(
        isinCode: String,
        name: String,
        instrumentType: InstrumentType,
        dateOfIssue: Option[LocalDateTime], // for non CCY
        dateOfMaturity: Option[LocalDateTime], // for Fixed Income
        lotSize: Option[Short],
        unitPrice: Option[Money], // for Equity
        couponRate: Option[Money], // for Fixed Income
        couponFrequency: Option[BigDecimal] // for Fixed Income
    ): EitherNec[String, Instrument] = {
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
