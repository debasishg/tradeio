package tradex.domain
package model

import java.time.LocalDateTime
import squants.market._
import cats.data.ValidatedNec
import cats.syntax.all._
import derevo.cats._
import derevo.circe.magnolia._
import derevo.derive
import account._

object balance {
  @derive(decoder, encoder, eqv, show)
  final case class Balance private (
      accountNo: AccountNo,
      amount: Money,
      currency: Currency,
      asOf: LocalDateTime
  )

  object Balance {
    def balance(
        accountNo: String,
        amount: Money,
        currency: Currency,
        asOf: LocalDateTime
    ): ValidatedNec[String, Balance] = {
      (
        Account.validateAccountNo(accountNo),
        validateAsOfDate(asOf)
      ).mapN { (ano, dt) =>
        Balance(ano, amount, currency, dt)
      }
    }

    private def validateAsOfDate(
        date: LocalDateTime
    ): ValidatedNec[String, LocalDateTime] =
      if (date.isAfter(today))
        s"Balance date [$date] cannot be later than today".invalidNec
      else date.validNec
  }
}
