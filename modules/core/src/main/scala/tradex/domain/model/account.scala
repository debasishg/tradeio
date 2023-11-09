package tradex.domain
package model

import scala.language.postfixOps
import java.time.LocalDateTime

import cats.data.ValidatedNec
import cats.syntax.all._

import squants.market._

import NewtypeRefinedOps._

import enumeratum._

import io.estatico.newtype.macros.newtype

import derevo.cats._
import derevo.circe.magnolia._
import derevo.derive
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection._
import eu.timepit.refined.boolean.AllOf
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._

import io.circe.refined._

import _root_.shapeless.::
import _root_.shapeless.HNil

object account {
  type AccountNoString = String Refined AllOf[
    MaxSize[W.`12`.T] ::
      MinSize[W.`5`.T] ::
      HNil
  ]

  @derive(decoder, encoder, eqv, show)
  @newtype case class AccountNo(value: AccountNoString)

  @derive(decoder, encoder, eqv, show)
  @newtype case class AccountName(value: NonEmptyString)

  @derive(decoder, encoder, eqv, show)
  sealed abstract class AccountType(override val entryName: String) extends EnumEntry

  @derive(decoder, encoder, eqv, show)
  object AccountType extends Enum[AccountType] {
    case object Trading    extends AccountType("Trading")
    case object Settlement extends AccountType("Settlement")
    case object Both       extends AccountType("Both")

    val values = findValues
  }

  @derive(decoder, encoder, eqv, show)
  final case class Account private (
      no: AccountNo,
      name: AccountName,
      dateOfOpen: LocalDateTime,
      dateOfClose: Option[LocalDateTime],
      accountType: AccountType,
      baseCurrency: Currency,
      tradingCurrency: Option[Currency],
      settlementCurrency: Option[Currency]
  )

  @derive(decoder, encoder, show)
  case class CreateAccount(
      no: String,
      name: String,
      openDate: Option[LocalDateTime],
      closeDate: Option[LocalDateTime],
      baseCcy: Currency,
      tradingCcy: Option[Currency],
      settlementCcy: Option[Currency],
      accountType: AccountType
  ) {
    def toDomain = {
      accountType match {
        case AccountType.Trading =>
          Account.tradingAccount(
            no,
            name,
            openDate,
            closeDate,
            baseCcy,
            tradingCcy.getOrElse(USD)
          )
        case AccountType.Settlement =>
          Account.settlementAccount(
            no,
            name,
            openDate,
            closeDate,
            baseCcy,
            settlementCcy.getOrElse(USD)
          )
        case AccountType.Both =>
          Account.tradingAndSettlementAccount(
            no,
            name,
            openDate,
            closeDate,
            baseCcy,
            tradingCcy.getOrElse(USD),
            settlementCcy.getOrElse(USD)
          )
      }
    }
  }

  object Account {
    def tradingAccount(
        no: String,
        name: String,
        openDate: Option[LocalDateTime],
        closeDate: Option[LocalDateTime],
        baseCcy: Currency,
        tradingCcy: Currency
    ): ValidatedNec[String, Account] = {
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
      }
    }

    def settlementAccount(
        no: String,
        name: String,
        openDate: Option[LocalDateTime],
        closeDate: Option[LocalDateTime],
        baseCcy: Currency,
        settlementCcy: Currency
    ): ValidatedNec[String, Account] = {
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
    ): ValidatedNec[String, Account] = {
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
      }
    }

    private[model] def validateAccountNo(
        no: String
    ): ValidatedNec[String, AccountNo] =
      validate[AccountNo](no).leftMap(
        _ :+ s"Account No has to be at least 5 characters long: found $no"
      )

    private[model] def validateAccountName(
        name: String
    ): ValidatedNec[String, AccountName] =
      validate[AccountName](name)
        .leftMap(_ :+ s"Account Name cannot be blank")

    private def validateOpenCloseDate(
        od: LocalDateTime,
        cd: Option[LocalDateTime]
    ): ValidatedNec[String, (LocalDateTime, Option[LocalDateTime])] =
      cd.map { c =>
        if (c isBefore od)
          s"Close date [$c] cannot be earlier than open date [$od]".invalidNec
        else (od, cd).validNec
      }.getOrElse { (od, cd).validNec }

    private def validateAccountAlreadyClosed(
        a: Account
    ): ValidatedNec[String, Account] = {
      if (a.dateOfClose isDefined)
        s"Account ${a.no} is already closed".invalidNec
      else a.validNec
    }

    private def validateCloseDate(
        a: Account,
        cd: LocalDateTime
    ): ValidatedNec[String, LocalDateTime] = {
      if (cd isBefore a.dateOfOpen)
        s"Close date [$cd] cannot be earlier than open date [${a.dateOfOpen}]".invalidNec
      else cd.validNec
    }

    def close(
        a: Account,
        closeDate: LocalDateTime
    ): ValidatedNec[String, Account] = {
      (validateAccountAlreadyClosed(a), validateCloseDate(a, closeDate))
        .mapN { (acc, _) =>
          acc.copy(dateOfClose = Some(closeDate))
        }
    }
  }
}
