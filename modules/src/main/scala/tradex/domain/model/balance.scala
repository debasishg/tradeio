package tradex.domain
package model

import java.time.LocalDateTime
import squants.market._
import cats.implicits._
import newtypes._
import common._
import account._

object balance {
  private[domain] final case class Balance private (
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
    ): ValidationResult[Balance] = {
      (
        Account.validateAccountNo(accountNo),
        validateAsOfDate(asOf)
      ).mapN { (ano, dt) =>
        Balance(ano, amount, currency, dt)
      }
    }

    private def validateAsOfDate(
        date: LocalDateTime
    ): ValidationResult[LocalDateTime] =
      if (date.isAfter(today))
        s"Balance date [$date] cannot be later than today".invalidNec
      else date.validNec
  }
}
