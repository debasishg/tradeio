package tradex.domain
package model

import java.time.LocalDateTime

import cats.implicits._

import squants.market._

import common._
import NewtypeRefinedOps._

import newtypes._
import enums._

object account {
  private[domain] final case class Account(
      no: AccountNo,
      name: AccountName,
      dateOfOpen: LocalDateTime,
      dateOfClose: Option[LocalDateTime],
      accountType: AccountType,
      baseCurrency: Currency,
      tradingCurrency: Option[Currency],
      settlementCurrency: Option[Currency]
  )

  object Account {
    def tradingAccount(
        no: String,
        name: String,
        openDate: Option[LocalDateTime],
        closeDate: Option[LocalDateTime],
        baseCcy: Currency,
        tradingCcy: Currency
    ): ErrorOr[Account] = {
      (
        validateAccountNo(no),
        validateAccountName(name),
        validateOpenCloseDate(openDate.getOrElse(today), closeDate)
      ).mapN { (n, nm, d) =>
        Account(
          n,
          nm,
          d._1,
          d._2,
          AccountType.Trading,
          baseCcy,
          tradingCcy.some,
          None
        )
      }.toEither
    }

    def settlementAccount(
        no: String,
        name: String,
        openDate: Option[LocalDateTime],
        closeDate: Option[LocalDateTime],
        baseCcy: Currency,
        settlementCcy: Currency
    ): ErrorOr[Account] = {
      (
        validateAccountNo(no),
        validateAccountName(name),
        validateOpenCloseDate(openDate.getOrElse(today), closeDate)
      ).mapN { (n, nm, d) =>
        Account(
          n,
          nm,
          d._1,
          d._2,
          AccountType.Settlement,
          baseCcy,
          None,
          settlementCcy.some
        )
      }.toEither
    }

    def tradingAndSettlementAccount(
        no: String,
        name: String,
        openDate: Option[LocalDateTime],
        closeDate: Option[LocalDateTime],
        baseCcy: Currency,
        tradingCcy: Currency,
        settlementCcy: Currency
    ): ErrorOr[Account] = {
      (
        validateAccountNo(no),
        validateAccountName(name),
        validateOpenCloseDate(openDate.getOrElse(today), closeDate)
      ).mapN { (n, nm, d) =>
        Account(
          n,
          nm,
          d._1,
          d._2,
          AccountType.Both,
          baseCcy,
          tradingCcy.some,
          settlementCcy.some
        )
      }.toEither
    }

    private[model] def validateAccountNo(
        no: String
    ): ValidationResult[AccountNo] =
      validate[AccountNo](no).toValidated.leftMap(
        _ :+ s"Account No has to be at least 5 characters long: found $no"
      )

    private[model] def validateAccountName(
        name: String
    ): ValidationResult[AccountName] =
      validate[AccountName](name).toValidated
        .leftMap(_ :+ s"Account Name cannot be blank")

    private def validateOpenCloseDate(
        od: LocalDateTime,
        cd: Option[LocalDateTime]
    ): ValidationResult[(LocalDateTime, Option[LocalDateTime])] =
      cd.map { c =>
          if (c isBefore od)
            s"Close date [$c] cannot be earlier than open date [$od]".invalidNec
          else (od, cd).validNec
        }
        .getOrElse { (od, cd).validNec }

    private def validateAccountAlreadyClosed(
        a: Account
    ): ValidationResult[Account] = {
      if (a.dateOfClose isDefined)
        s"Account ${a.no} is already closed".invalidNec
      else a.validNec
    }

    private def validateCloseDate(
        a: Account,
        cd: LocalDateTime
    ): ValidationResult[LocalDateTime] = {
      if (cd isBefore a.dateOfOpen)
        s"Close date [$cd] cannot be earlier than open date [${a.dateOfOpen}]".invalidNec
      else cd.validNec
    }

    def close(a: Account, closeDate: LocalDateTime): ErrorOr[Account] = {
      (validateAccountAlreadyClosed(a), validateCloseDate(a, closeDate)).mapN {
        (acc, _) =>
          acc.copy(dateOfClose = Some(closeDate))
      }.toEither
    }
  }
}
