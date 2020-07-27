package tradex.domain
package model

import java.time.LocalDateTime

import cats.data.{ EitherNec, NonEmptyChain }
import cats.implicits._

import squants.market._

import common._
import NewtypeRefinedOps._

import newtypes._
import enums._

object account {
  private[domain] final case class Account private (
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
    ): EitherNec[String, Account] = {
      (
        validateAccountNo(no),
        validateAccountName(name),
        validateOpenCloseDate(openDate.getOrElse(today), closeDate)
      ).parMapN { (n, nm, d) =>
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
      }
    }

    def settlementAccount(
        no: String,
        name: String,
        openDate: Option[LocalDateTime],
        closeDate: Option[LocalDateTime],
        baseCcy: Currency,
        settlementCcy: Currency
    ): EitherNec[String, Account] = {
      (
        validateAccountNo(no),
        validateAccountName(name),
        validateOpenCloseDate(openDate.getOrElse(today), closeDate)
      ).parMapN { (n, nm, d) =>
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
      }
    }

    def tradingAndSettlementAccount(
        no: String,
        name: String,
        openDate: Option[LocalDateTime],
        closeDate: Option[LocalDateTime],
        baseCcy: Currency,
        tradingCcy: Currency,
        settlementCcy: Currency
    ): EitherNec[String, Account] = {
      (
        validateAccountNo(no),
        validateAccountName(name),
        validateOpenCloseDate(openDate.getOrElse(today), closeDate)
      ).parMapN { (n, nm, d) =>
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
      }
    }

    private[model] def validateAccountNo(
        no: String
    ): EitherNec[String, AccountNo] =
      validate[AccountNo](no).leftMap(
        _ :+ s"Account No has to be at least 5 characters long: found $no"
      )

    private[model] def validateAccountName(
        name: String
    ): EitherNec[String, AccountName] =
      validate[AccountName](name)
        .leftMap(_ :+ s"Account Name cannot be blank")

    private def validateOpenCloseDate(
        od: LocalDateTime,
        cd: Option[LocalDateTime]
    ): EitherNec[String, (LocalDateTime, Option[LocalDateTime])] =
      cd.map { c =>
          if (c isBefore od)
            Left(NonEmptyChain.one(s"Close date [$c] cannot be earlier than open date [$od]"))
          else Right((od, cd))
        }
        .getOrElse { Right((od, cd)) }

    private def validateAccountAlreadyClosed(
        a: Account
    ): EitherNec[String, Account] = {
      if (a.dateOfClose isDefined)
        Left(NonEmptyChain.one(s"Account ${a.no} is already closed"))
      else Right(a)
    }

    private def validateCloseDate(
        a: Account,
        cd: LocalDateTime
    ): EitherNec[String, LocalDateTime] = {
      if (cd isBefore a.dateOfOpen)
        Left(NonEmptyChain.one(s"Close date [$cd] cannot be earlier than open date [${a.dateOfOpen}]"))
      else Right(cd)
    }

    def close(a: Account, closeDate: LocalDateTime): EitherNec[String, Account] = {
      (validateAccountAlreadyClosed(a), validateCloseDate(a, closeDate)).parMapN {
        (acc, _) =>
          acc.copy(dateOfClose = Some(closeDate))
      }
    }
  }
}
