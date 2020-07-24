package tradex.domain
package model

import java.time.LocalDateTime

import cats.implicits._

import squants.market._

import common._
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
    ): ValidationResult[ISINCode] = validate[ISINCode](isin).toValidated

    private[model] def validateInstrumentName(
        name: String
    ): ValidationResult[InstrumentName] =
      validate[InstrumentName](name).toValidated

    private[domain] def instrument(
        isinCode: String,
        name: String,
        instrumentType: InstrumentType,
        dateOfIssue: Option[LocalDateTime], // for non CCY
        dateOfMaturity: Option[LocalDateTime], // for Fixed Income
        lotSize: LotSize,
        unitPrice: Option[Money], // for Equity
        couponRate: Option[Money], // for Fixed Income
        couponFrequency: Option[BigDecimal] // for Fixed Income
    ): ValidationResult[Instrument] = {
      (
        validateISINCode(isinCode),
        validateInstrumentName(name)
      ).mapN { (isin, name) =>
        Instrument(
          isin,
          name,
          instrumentType,
          dateOfIssue,
          dateOfMaturity,
          lotSize,
          unitPrice,
          couponRate,
          couponFrequency
        )
      }
    }
  }
}
