package org.github.domain
package repository
package interpreter.skunk

import java.time.LocalDateTime

import cats.implicits._
import cats.effect._
import cats.effect.concurrent.Ref

import skunk._
import skunk.data.Type
import skunk.codec.all._
import skunk.implicits._

import squants.market._
import squants.market.defaultMoneyContext

import model.newtypes._
import model.enums._
import model.account._
import Account._
import ext.skunkx._

final class AccountRepositoryInterpreter[M[_]: Sync] private (
  sessionPool: Resource[M, Session[M]]) extends AccountRepository[M] {
  def query(no: AccountNo): M[Option[Account]] = ???

  def store(a: Account): M[Account] = ???

  def query(openedOn: LocalDateTime): M[List[Account]] = ???

  def all: M[List[Account]] = ???

  def allClosed(closeDate: Option[LocalDateTime]): M[List[Account]] = ???

  def allAccountsOfType(accountType: AccountType): M[List[Account]] = ???
}

private object AccountQueries {

  // A codec that maps Postgres type `accountType` to Scala type `AccountType`
  val accountType = enum(AccountType, Type("accounttype"))
  implicit val moneyContext = defaultMoneyContext

  val decoder: Decoder[Account] =
    (varchar ~ varchar ~ accountType ~ timestamp ~ timestamp.opt ~ varchar ~ varchar.opt ~ varchar.opt).map {
      case no ~ nm ~ tp ~ dp ~ dc ~ bc ~ tc ~ sc =>
        tp match {
          case AccountType.Trading =>
            tradingAccount(
              AccountNo(no), 
              AccountName(nm), 
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
              AccountNo(no), 
              AccountName(nm), 
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
              AccountNo(no), 
              AccountName(nm), 
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
        WHERE a.no = ${varchar.cimap[AccountNo]}
       """.query(decoder)

  val selectByOpenedDate: Query[LocalDateTime, Account] =
    sql"""
        SELECT a.no, a.name, a.type, a.dateOfOpen, a.dateOfClose, a.baseCurrency, a.tradingCurrency, a.settlementCurrency
        FROM accounts AS a
        WHERE a.dateOfOpen = $timestamp
       """.query(decoder)

  val selectAll: Query[Void, Account] =
    sql"""
        SELECT a.no, a.name, a.type, a.dateOfOpen, a.dateOfClose, a.baseCurrency, a.tradingCurrency, a.settlementCurrency
        FROM accounts AS a
       """.query(decoder)

  val insertAccount: Command[Account] =
    sql"""
        INSERT INTO accounts
        VALUES ($varchar, $varchar, $accountType, $timestamp, ${timestamp.opt}, $varchar, ${varchar.opt}, ${varchar.opt})
       """.command.contramap {

      case a => a match {
        case Account(no, nm, dop, doc, AccountType.Both, bc, tc, sc) => 
          no.value ~ nm.value ~ AccountType.Both ~ dop ~ doc ~ bc.toString ~ Option(tc.toString) ~ Option(sc.toString)

        case Account(no, nm, dop, doc, AccountType.Trading, bc, tc, _) => 
          no.value ~ nm.value ~ AccountType.Trading ~ dop ~ doc ~ bc.toString ~ Option(tc.toString) ~ None
         
        case Account(no, nm, dop, doc, AccountType.Settlement, bc, _, sc) => 
          no.value ~ nm.value ~ AccountType.Settlement ~ dop ~ doc ~ bc.toString ~ None ~ Option(sc.toString)
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

      case a => a match {
        case Account(no, nm, dop, doc, AccountType.Trading, bc, tc, _) => 
          nm.value ~ AccountType.Trading ~ dop ~ doc ~ bc.toString ~ Option(tc.toString) ~ None ~ no.value
         
        case Account(no, nm, dop, doc, AccountType.Settlement, bc, _, sc) => 
          nm.value ~ AccountType.Settlement ~ dop ~ doc ~ bc.toString ~ None ~ Option(sc.toString) ~ no.value
         
        case Account(no, nm, dop, doc, AccountType.Both, bc, tc, sc) => 
          nm.value ~ AccountType.Settlement ~ dop ~ doc ~ bc.toString ~ Option(tc.toString) ~ Option(sc.toString) ~ no.value
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
          tradingCurrency      = EXCLUDED.tradingCurrency
          settlementCurrency   = EXCLUDED.settlementCurrency
       """.command.contramap {

      case a => a match {
        case Account(no, nm, dop, doc, AccountType.Trading, bc, tc, _) => 
          no.value ~ nm.value ~ AccountType.Trading ~ dop ~ doc ~ bc.toString ~ Option(tc.toString) ~ None
         
        case Account(no, nm, dop, doc, AccountType.Settlement, bc, _, sc) => 
          no.value ~ nm.value ~ AccountType.Settlement ~ dop ~ doc ~ bc.toString ~ None ~ Option(sc.toString)
         
        case Account(no, nm, dop, doc, AccountType.Both, bc, tc, sc) => 
          no.value ~ nm.value ~ AccountType.Settlement ~ dop ~ doc ~ bc.toString ~ Option(tc.toString) ~ Option(sc.toString)
      }
    }
}

// Smart constructor 
object AccountRepositoryInterpreter {
  def make[M[_]: Sync](
    sessionPool: Resource[M, Session[M]]
  ): M[AccountRepositoryInterpreter[M]] = Sync[M].delay(new AccountRepositoryInterpreter[M](sessionPool))
}
