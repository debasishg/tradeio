package tradex.domain
package repository

import java.time.LocalDate

import cats.syntax.all._
import cats.effect._

import skunk._
import skunk.data.Type
import skunk.codec.all._
import skunk.implicits._

import squants.market._

import model.account._
import Account._
import codecs._

trait AccountRepository[F[_]] {
  /** query by account number */
  def query(no: AccountNo): F[Option[Account]]

  /** store */
  def store(a: Account): F[Account]

  /** query by opened date */
  def query(openedOn: LocalDate): F[List[Account]]

  /** all accounts */
  def all: F[List[Account]]

  /** all closed accounts, if date supplied then all closed after that date */
  def allClosed(closeDate: Option[LocalDate]): F[List[Account]]

  /** all accounts trading / settlement / both */
  def allAccountsOfType(accountType: AccountType): F[List[Account]]
}

object AccountRepository {
  def make[F[_]: Concurrent](
      postgres: Resource[F, Session[F]]
  ): AccountRepository[F] =
    new AccountRepository[F] {
      import AccountRepositorySQL._

      def query(no: AccountNo): F[Option[Account]] =
        postgres.use { session =>
          session.prepare(selectByAccountNo).use { ps =>
            ps.option(no)
          }
        }

      def store(a: Account): F[Account] =
        postgres.use { session =>
          session.prepare(upsertAccount).use { cmd =>
            cmd.execute(a).void.map(_ => a)
          }
        }

      def query(openedOn: LocalDate): F[List[Account]] =
        postgres.use { session =>
          session.prepare(selectByOpenedDate).use { ps =>
            ps.stream(openedOn, 1024).compile.toList
          }
        }

      def all: F[List[Account]] = postgres.use(_.execute(selectAll))

      def allClosed(closeDate: Option[LocalDate]): F[List[Account]] =
        postgres.use { session =>
          closeDate
            .map { cd =>
              session.prepare(selectClosedAfter).use { ps =>
                ps.stream(cd, 1024).compile.toList
              }
            }
            .getOrElse {
              session.execute(selectAllClosed)
            }
        }

      def allAccountsOfType(accountType: AccountType): F[List[Account]] =
        postgres.use { session =>
          session.prepare(selectByAccountType).use { ps =>
            ps.stream(accountType, 1024).compile.toList
          }
        }
    }
}

private object AccountRepositorySQL {
  // A codec that maps Postgres type `accountType` to Scala type `AccountType`
  val accountType = enum(AccountType, Type("accounttype"))

  val decoder: Decoder[Account] =
    (varchar ~ varchar ~ accountType ~ timestamp ~ timestamp.opt ~ varchar ~ varchar.opt ~ varchar.opt)
      .map {
        case no ~ nm ~ tp ~ dp ~ dc ~ bc ~ tc ~ sc =>
          tp match {
            case AccountType.Trading =>
              tradingAccount(
                no,
                nm,
                Option(dp),
                dc,
                Currency(bc).get,
                Currency(tc.get).get
              ).fold(
                exs => throw new Exception(exs.toList.mkString("/")),
                tac => tac
              )
            case AccountType.Settlement =>
              settlementAccount(
                no,
                nm,
                Option(dp),
                dc,
                Currency(bc).get,
                Currency(sc.get).get
              ).fold(
                exs => throw new Exception(exs.toList.mkString("/")),
                sac => sac
              )
            case AccountType.Both =>
              tradingAndSettlementAccount(
                no,
                nm,
                Option(dp),
                dc,
                Currency(bc).get,
                Currency(tc.get).get,
                Currency(sc.get).get
              ).fold(
                exs => throw new Exception(exs.toList.mkString("/")),
                sac => sac
              )
          }
      }

  val selectByAccountNo: Query[AccountNo, Account] =
    sql"""
        SELECT a.no, a.name, a.type, a.dateOfOpen, a.dateOfClose, a.baseCurrency, a.tradingCurrency, a.settlementCurrency
        FROM accounts AS a
        WHERE a.no = $accountNo
       """.query(decoder)

  val selectByOpenedDate: Query[LocalDate, Account] =
    sql"""
        SELECT a.no, a.name, a.type, a.dateOfOpen, a.dateOfClose, a.baseCurrency, a.tradingCurrency, a.settlementCurrency
        FROM accounts AS a
        WHERE DATE(a.dateOfOpen) = $date
       """.query(decoder)

  val selectByAccountType: Query[AccountType, Account] =
    sql"""
        SELECT a.no, a.name, a.type, a.dateOfOpen, a.dateOfClose, a.baseCurrency, a.tradingCurrency, a.settlementCurrency
        FROM accounts AS a
        WHERE a.type = $accountType
       """.query(decoder)

  val selectAll: Query[Void, Account] =
    sql"""
        SELECT a.no, a.name, a.type, a.dateOfOpen, a.dateOfClose, a.baseCurrency, a.tradingCurrency, a.settlementCurrency
        FROM accounts AS a
       """.query(decoder)

  val selectClosedAfter: Query[LocalDate, Account] =
    sql"""
        SELECT a.no, a.name, a.type, a.dateOfOpen, a.dateOfClose, a.baseCurrency, a.tradingCurrency, a.settlementCurrency
        FROM accounts AS a
        WHERE a.dateOfClose >= $date
       """.query(decoder)

  val selectAllClosed: Query[Void, Account] =
    sql"""
        SELECT a.no, a.name, a.type, a.dateOfOpen, a.dateOfClose, a.baseCurrency, a.tradingCurrency, a.settlementCurrency
        FROM accounts AS a
        WHERE a.dateOfClose IS NOT NULL
       """.query(decoder)

  val insertAccount: Command[Account] =
    sql"""
        INSERT INTO accounts
        VALUES ($varchar, $varchar, $accountType, $timestamp, ${timestamp.opt}, $varchar, ${varchar.opt}, ${varchar.opt})
       """.command.contramap {
      case a =>
        a match {
          case Account(no, nm, dop, doc, AccountType.Both, bc, tc, sc) =>
            no.value.value ~ nm.value.value ~ AccountType.Both ~ dop ~ doc ~ bc.toString ~ Option(
              tc.toString
            ) ~ Option(sc.toString)

          case Account(no, nm, dop, doc, AccountType.Trading, bc, tc, _) =>
            no.value.value ~ nm.value.value ~ AccountType.Trading ~ dop ~ doc ~ bc.toString ~ Option(
              tc.toString
            ) ~ None

          case Account(no, nm, dop, doc, AccountType.Settlement, bc, _, sc) =>
            no.value.value ~ nm.value.value ~ AccountType.Settlement ~ dop ~ doc ~ bc.toString ~ None ~ Option(
              sc.toString
            )
        }
    }

  val updateAccount: Command[Account] =
    sql"""
        UPDATE accounts SET
          name                = $varchar,
          type                = $accountType,
          dateOfOpen          = $timestamp,
          dateOfClose         = ${timestamp.opt},
          baseCurrency        = $varchar,
          tradingCurrency     = ${varchar.opt},
          settlementCurrency  = ${varchar.opt}
        WHERE no = $varchar
       """.command.contramap {
      case a =>
        a match {
          case Account(no, nm, dop, doc, AccountType.Trading, bc, tc, _) =>
            nm.value.value ~ AccountType.Trading ~ dop ~ doc ~ bc.toString ~ Option(
              tc.toString
            ) ~ None ~ no.value.value

          case Account(no, nm, dop, doc, AccountType.Settlement, bc, _, sc) =>
            nm.value.value ~ AccountType.Settlement ~ dop ~ doc ~ bc.toString ~ None ~ Option(
              sc.toString
            ) ~ no.value.value

          case Account(no, nm, dop, doc, AccountType.Both, bc, tc, sc) =>
            nm.value.value ~ AccountType.Settlement ~ dop ~ doc ~ bc.toString ~ Option(
              tc.toString
            ) ~ Option(sc.toString) ~ no.value.value
        }
    }

  val upsertAccount: Command[Account] =
    sql"""
        INSERT INTO accounts
        VALUES ($varchar, $varchar, $accountType, $timestamp, ${timestamp.opt}, $varchar, ${varchar.opt}, ${varchar.opt})
        ON CONFLICT(no) DO UPDATE SET
          name                 = EXCLUDED.name,
          type                 = EXCLUDED.type,
          dateOfOpen           = EXCLUDED.dateOfOpen,
          dateOfClose          = EXCLUDED.dateOfClose,
          baseCurrency         = EXCLUDED.baseCurrency,
          tradingCurrency      = EXCLUDED.tradingCurrency,
          settlementCurrency   = EXCLUDED.settlementCurrency
       """.command.contramap {
      case a =>
        a match {
          case Account(no, nm, dop, doc, AccountType.Trading, bc, tc, _) =>
            no.value.value ~ nm.value.value ~ AccountType.Trading ~ dop ~ doc ~ bc.toString ~ Option(
              tc.toString
            ) ~ None

          case Account(no, nm, dop, doc, AccountType.Settlement, bc, _, sc) =>
            no.value.value ~ nm.value.value ~ AccountType.Settlement ~ dop ~ doc ~ bc.toString ~ None ~ Option(
              sc.toString
            )

          case Account(no, nm, dop, doc, AccountType.Both, bc, tc, sc) =>
            no.value.value ~ nm.value.value ~ AccountType.Settlement ~ dop ~ doc ~ bc.toString ~ Option(
              tc.toString
            ) ~ Option(sc.toString)
        }
    }
}
