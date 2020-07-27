package tradex.domain
package trading

import java.time.LocalDate

import cats.data.NonEmptyList
import model.trade._
import model.balance._
import scala.util.control.NoStackTrace

trait Accounting[F[_]] {
  def postBalance(trade: Trade): F[Balance]
  def postBalance(trades: NonEmptyList[Trade]): F[NonEmptyList[Balance]]
  def getBalance(accountNo: String): F[Option[Balance]]
  def getBalanceByDate(date: LocalDate): F[List[Balance]]
}

object Accounting {
  case class AccountingError(cause: String) extends NoStackTrace
}
