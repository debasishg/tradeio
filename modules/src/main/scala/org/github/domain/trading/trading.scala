package org.github.domain
package trading

import cats.data.NonEmptyList

import model.account.Account
import model.execution.Execution
import model.order.Order
import model.trade.Trade
import model.market.Market

trait Trading[F[_]] {
  /**
    * Create a list of `Order` from client orders read from a stream
    * as a csv file.
    *
    * @param csvOrder client order in csv format
    * @return a List of `Order` under the effect `F`
    */
  def orders(csvOrder: String): F[List[Order]]

  /**
    * Execute an `Order` in the `Market` and book the execution in the 
    * broker account supplied.
    *
    * @param order the order to execute
    * @param market the market of execution
    * @param brokerAccount the broker account where the execution will be booked
    * @return a List of `Execution` generated from the `Order`
    */
  def execute(order: Order, market: Market, brokerAccount: Account): F[NonEmptyList[Execution]]

  /**
    * Allocate the `Execution` equally between the client accounts generating
    * a list of `Trade`s.
    *
    * @param execution the execution to allocate
    * @param clientAccounts the client accounts for which `Trade` will be generated
    * @return a list of `Trade`
    */
  def allocate(execution: Execution, clientAccounts: List[Account]): F[List[Trade]]
}
