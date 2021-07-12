package tradex.domain
package repository
package interpreter.skunk

import java.time.LocalDate

import cats.implicits._
import cats.effect._

import skunk._
import skunk.data.Type
import skunk.codec.all._
import skunk.implicits._

import squants.market._

import model.enums._
import model.account._
import Account._

final class AccountRepositoryInterpreter[M[_]: Concurrent] private (
    postgres: Resource[M, Session[M]]
) extends AccountRepository[M] {
  import AccountQueries._

  def query(no: String): M[Option[Account]] =
    postgres.use { session =>
      session.prepare(selectByAccountNo).use { ps =>
        ps.option(no)
      }
    }

  def store(a: Account): M[Account] =
    postgres.use { session =>
      session.prepare(upsertAccount).use { cmd =>
        cmd.execute(a).void.map(_ => a)
      }
    }

  def query(openedOn: LocalDate): M[List[Account]] =
    postgres.use { session =>
      session.prepare(selectByOpenedDate).use { ps =>
        ps.stream(openedOn, 1024).compile.toList
      }
    }

  def all: M[List[Account]] = postgres.use(_.execute(selectAll))

  def allClosed(closeDate: Option[LocalDate]): M[List[Account]] =
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

  def allAccountsOfType(accountType: AccountType): M[List[Account]] =
    postgres.use { session =>
      session.prepare(selectByAccountType).use { ps =>
        ps.stream(accountType, 1024).compile.toList
      }
    }
}

private object AccountQueries {
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

  val selectByAccountNo: Query[String, Account] =
    sql"""
        SELECT a.no, a.name, a.type, a.dateOfOpen, a.dateOfClose, a.baseCurrency, a.tradingCurrency, a.settlementCurrency
        FROM accounts AS a
        WHERE a.no = $varchar
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

// Smart constructor
object AccountRepositoryInterpreter {
  def make[M[_]: Concurrent](
      postgres: Resource[M, Session[M]]
  ): AccountRepositoryInterpreter[M] =
    new AccountRepositoryInterpreter[M](postgres)
}
