package tradex.domain
package repository

import java.time.LocalDate

import cats.data.NonEmptyList
import cats.syntax.all._
import cats.effect._

import skunk._
import skunk.codec.all._
import skunk.implicits._

import model.balance._
import model.account.AccountNo
import codecs._

trait BalanceRepository[F[_]] {
  /** query by account number */
  def query(no: AccountNo): F[Option[Balance]]

  /** store */
  def store(a: Balance): F[Balance]

  /** store many balances */
  // def store(balances: NonEmptyList[Balance]): M[Unit]

  /** query all balances that have amount as of this date */
  /** asOf date <= this date */
  def query(date: LocalDate): F[List[Balance]]

  /** all balances */
  def all: F[List[Balance]]
}

object BalanceRepository {
  def make[F[_]: Concurrent](
      postgres: Resource[F, Session[F]]
  ): BalanceRepository[F] =
    new BalanceRepository[F] {
      import BalanceRepositorySQL._

      def query(no: AccountNo): F[Option[Balance]] =
        postgres.use { session =>
          session.prepare(selectByAccountNo).use { ps =>
            ps.option(no)
          }
        }

      def store(b: Balance): F[Balance] =
        postgres.use { session =>
          session.prepare(upsertBalance).use { cmd =>
            cmd.execute(b).void.map(_ => b)
          }
        }

      def store(balances: NonEmptyList[Balance]): F[Unit] = ???

      def query(date: LocalDate): F[List[Balance]] =
        postgres.use { session =>
          session.prepare(selectByDate).use { ps =>
            ps.stream(date, 1024).compile.toList
          }
        }

      def all: F[List[Balance]] = postgres.use(_.execute(selectAll))
    }
}

private object BalanceRepositorySQL {
  val decoder: Decoder[Balance] =
    (accountNo ~ money ~ timestamp ~ currency)
      .map {
        case ano ~ amt ~ asOf ~ ccy =>
          Balance(ano, amt, ccy, asOf)
      }

  val encoder: Encoder[Balance] =
    (accountNo ~ money ~ timestamp ~ currency).values
      .contramap(
        (b: Balance) => b.accountNo ~ b.amount ~ b.asOf ~ b.currency
      )

  val selectByAccountNo: Query[AccountNo, Balance] =
    sql"""
        SELECT b.accountNo, b.amount, b.asOf, b.currency
        FROM balance AS b
        WHERE b.accountNo = $accountNo
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
        VALUES $encoder
        ON CONFLICT(accountNo) DO UPDATE SET
          amount    = EXCLUDED.amount,
          asOf      = EXCLUDED.asOf,
          currency  = EXCLUDED.currency
       """.command
}
