package tradex.domain
package trading

import java.time.LocalDate

import cats.data.NonEmptyList

import model.account.Account
import model.execution.Execution
import model.order.Order
import model.trade.Trade
import model.market.Market
import model.newtypes._
import scala.util.control.NoStackTrace

trait Trading[F[_]] {
  /**
    * Find accounts opened on the date specified
    *
    * @param openDate the date when account was opened
    * @return a list of `Account` under the effect `F`
    */
  def getAccountsOpenedOn(openDate: LocalDate): F[List[Account]]

  /**
    * Find the list of trades for the supplied client account no and (optionally)
    * the trade date.
    *
    * @param forAccountNo the client accountNo
    * @param forDate the trade date
    * @return a list of `Trade` under the effect `F`
    */
  def getTrades(
      forAccountNo: String,
      forDate: Option[LocalDate] = None
  ): F[List[Trade]]

  /**
    * Create a list of `Order` from client orders read from a stream
    * as a csv file.
    *
    * @param csvOrder client order in csv format
    * @return a List of `Order` under the effect `F`
    */
  def orders(csvOrder: String): F[NonEmptyList[Order]]

  /**
    * Execute an `Order` in the `Market` and book the execution in the
    * broker account supplied.
    *
    * @param orders the orders to execute
    * @param market the market of execution
    * @param brokerAccount the broker account where the execution will be booked
    * @return a List of `Execution` generated from the `Order`
    */
  def execute(
      orders: NonEmptyList[Order],
      market: Market,
      brokerAccountNo: AccountNo
  ): F[NonEmptyList[Execution]]

  /**
    * Allocate the `Execution` equally between the client accounts generating
    * a list of `Trade`s.
    *
    * @param executions the executions to allocate
    * @param clientAccounts the client accounts for which `Trade` will be generated
    * @return a list of `Trade`
    */
  def allocate(
      executions: NonEmptyList[Execution],
      clientAccounts: NonEmptyList[AccountNo]
  ): F[NonEmptyList[Trade]]
}

object Trading {
  case class OrderingError(cause: String) extends NoStackTrace
  case class ExecutionError(cause: String) extends NoStackTrace
  case class AllocationError(cause: String) extends NoStackTrace
}
