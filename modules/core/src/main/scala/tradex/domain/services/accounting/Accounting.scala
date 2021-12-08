package tradex.domain
package services.accounting

import scala.util.control.NoStackTrace

import java.time.LocalDate

import cats.data.NonEmptyList
import cats.syntax.all._

import model.trade.Trade
import model.balance.Balance
import model.account.AccountNo

import repository._

trait Accounting[F[_]] {
  def postBalance(trade: Trade): F[Balance]
  def postBalance(trades: NonEmptyList[Trade]): F[NonEmptyList[Balance]]
  def getBalance(accountNo: AccountNo): F[Option[Balance]]
  def getBalanceByDate(date: LocalDate): F[List[Balance]]
}

object Accounting {
  case class AccountingError(cause: String) extends NoStackTrace

  def make[F[+_]: MonadThrowable](
      balanceRepository: BalanceRepository[F]
  ): Accounting[F] =
    new Accounting[F] {
      private final val F = implicitly[MonadThrowable[F]]
      def postBalance(trade: Trade): F[Balance] = {
        val action = trade.netAmount
          .map { amt =>
            for {
              balance <- balanceRepository.store(
                Balance(trade.accountNo, amt, amt.currency, today)
              )
            } yield balance
          }
          .getOrElse(
            F.raiseError(
              new Throwable(
                s"Net amount for trade $trade not available for posting"
              )
            )
          )
        action.adaptError { case e =>
          Accounting.AccountingError(
            Option(e.getMessage()).getOrElse("Unknown error in posting balance for accounting")
          )
        }
      }

      def postBalance(trades: NonEmptyList[Trade]): F[NonEmptyList[Balance]] =
        trades.map(postBalance).sequence

      def getBalance(accountNo: AccountNo): F[Option[Balance]] = {
        balanceRepository.query(accountNo).adaptError { case e =>
          Accounting.AccountingError(
            Option(e.getMessage()).getOrElse("Unknown error in fetching balance")
          )
        }
      }

      def getBalanceByDate(date: LocalDate): F[List[Balance]] = {
        balanceRepository.query(date).adaptError { case e =>
          Accounting.AccountingError(
            Option(e.getMessage()).getOrElse("Unknown error in fetching balance by date")
          )
        }
      }
    }
}
