package tradex.domain
package trading

import java.time.LocalDate

import cats.data.NonEmptyList
import cats.implicits._
import cats.mtl._

import common._
import model.trade.Trade
import model.balance.Balance

import repository._

class AccountingInterpreter[M[_]: MonadThrowable](
    implicit B: ApplicativeAsk[M, BalanceRepository[M]]
) extends Accounting[M] {
  private final val ev = implicitly[MonadThrowable[M]]

  def postBalance(trade: Trade): M[Balance] = {
    trade.netAmount
      .map { amt =>
        for {
          repo <- B.ask
          balance <- repo.store(
            Balance(trade.accountNo, amt, amt.currency, today)
          )
        } yield balance
      }
      .getOrElse(
        ev.raiseError(
          new Throwable(
            s"Net amount for trade $trade not available for posting"
          )
        )
      )
  }

  def postBalance(trades: NonEmptyList[Trade]): M[NonEmptyList[Balance]] =
    trades.map(postBalance).sequence

  def getBalance(accountNo: String): M[Option[Balance]] =
    for {
      repo <- B.ask
      balance <- repo.query(accountNo)
    } yield balance

  def getBalanceByDate(date: LocalDate): M[List[Balance]] =
    for {
      repo <- B.ask
      balance <- repo.query(date)
    } yield balance
}
