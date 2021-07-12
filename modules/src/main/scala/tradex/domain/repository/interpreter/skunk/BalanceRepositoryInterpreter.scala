package tradex.domain
package repository
package interpreter.skunk

import java.time.LocalDate

import cats.data.NonEmptyList
import cats.implicits._
import cats.effect._

import skunk._
import skunk.codec.all._
import skunk.implicits._

import squants.market._

import model.balance._

final class BalanceRepositoryInterpreter[M[_]: Concurrent] private (
    postgres: Resource[M, Session[M]]
) extends BalanceRepository[M] {
  import BalanceQueries._

  def query(no: String): M[Option[Balance]] =
    postgres.use { session =>
      session.prepare(selectByAccountNo).use { ps =>
        ps.option(no)
      }
    }

  def store(b: Balance): M[Balance] =
    postgres.use { session =>
      session.prepare(upsertBalance).use { cmd =>
        cmd.execute(b).void.map(_ => b)
      }
    }

  def store(balances: NonEmptyList[Balance]): M[Unit] = ???

  def query(date: LocalDate): M[List[Balance]] =
    postgres.use { session =>
      session.prepare(selectByDate).use { ps =>
        ps.stream(date, 1024).compile.toList
      }
    }

  def all: M[List[Balance]] = postgres.use(_.execute(selectAll))
}

private object BalanceQueries {
  val decoder: Decoder[Balance] =
    (varchar ~ numeric ~ timestamp ~ varchar)
      .map {
        case ano ~ amt ~ asOf ~ ccy =>
          Balance
            .balance(
              ano,
              Money(amt),
              Currency(ccy).get,
              asOf
            )
            .fold(
              exs => throw new Exception(exs.toList.mkString("/")),
              identity
            )
      }

  val encoder: Encoder[Balance] =
    (varchar ~ numeric ~ timestamp ~ varchar).values
      .contramap(
        (b: Balance) =>
          b.accountNo.value.value ~ BigDecimal(b.amount.value) ~ b.asOf ~ b.currency.toString
      )

  val selectByAccountNo: Query[String, Balance] =
    sql"""
        SELECT b.accountNo, b.amount, b.asOf, b.currency
        FROM balance AS b
        WHERE b.accountNo = $varchar
       """.query(decoder)

  val selectByDate: Query[LocalDate, Balance] =
    sql"""
        SELECT b.accountNo, b.amount, b.asOf, b.currency
        FROM balance AS b
        WHERE DATE(a.asOf) <= $date
       """.query(decoder)

  val selectAll: Query[Void, Balance] =
    sql"""
        SELECT b.accountNo, b.amount, b.asOf, b.currency
        FROM balance AS b
       """.query(decoder)

  val insertBalance: Command[Balance] =
    sql"""
        INSERT INTO balance (accountNo, amount, asOf, currency)
        VALUES $encoder
       """.command

  val upsertBalance: Command[Balance] =
    sql"""
        INSERT INTO balance (accountNo, amount, asOf, currency)
        VALUES ($varchar, $numeric, $timestamp, $varchar)
        ON CONFLICT(accountNo) DO UPDATE SET
          amount    = EXCLUDED.amount,
          asOf      = EXCLUDED.asOf,
          currency  = EXCLUDED.currency
       """.command.contramap {
      case b =>
        b match {
          case Balance(accountNo, amount, ccy, asOf) =>
            accountNo.value.value ~ BigDecimal(amount.value) ~ asOf ~ ccy.toString
        }
    }
}

// Smart constructor
object BalanceRepositoryInterpreter {
  def make[M[_]: Concurrent](
      postgres: Resource[M, Session[M]]
  ): BalanceRepositoryInterpreter[M] =
    new BalanceRepositoryInterpreter[M](postgres)
}
