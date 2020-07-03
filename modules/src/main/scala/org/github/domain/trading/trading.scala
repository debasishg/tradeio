package org.github.domain
package trading

import cats.data.NonEmptyList

import model.account.Account
import model.execution.Execution
import model.order.Order
import model.trade.Trade
import model.market.Market
import model.newtypes._

trait Trading[F[_]] {
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
  def execute(orders: NonEmptyList[Order], market: Market, brokerAccountNo: AccountNo): F[NonEmptyList[Execution]]

  /**
    * Allocate the `Execution` equally between the client accounts generating
    * a list of `Trade`s.
    *
    * @param executions the executions to allocate
    * @param clientAccounts the client accounts for which `Trade` will be generated
    * @return a list of `Trade`
    */
  def allocate(executions: NonEmptyList[Execution], clientAccounts: NonEmptyList[AccountNo]): F[NonEmptyList[Trade]]
}
