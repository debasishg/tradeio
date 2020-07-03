package org.github.domain
package trading

import cats.MonadError
import cats.data.NonEmptyList
import cats.implicits._

import common._
import model.account.Account
import model.execution.Execution
import model.order.Order
import model.trade.Trade
import model.market.Market
import model.newtypes._

class TradingInterpreter[M[_]: MonadThrowable] extends Trading[M] {
  private final val ev = implicitly[MonadThrowable[M]]

  def orders(csvOrder: String): M[List[Order]] = {
    ordering
      .createOrders(csvOrder)
      .fold(
        nec => ev.raiseError(new Throwable(nec.toNonEmptyList.toList.mkString("/"))),
        ev.pure(_)
      )
  }

  def execute(order: Order, market: Market, brokerAccount: Account): M[NonEmptyList[Execution]] = ev.pure {
    order.items.map { item =>
      Execution(
        brokerAccount.no, 
        item.instrument, 
        ReferenceNo("e-123"), 
        market, 
        item.unitPrice, 
        item.quantity
      )
    }
  }

  def allocate(execution: Execution, clientAccounts: List[Account]): M[List[Trade]] = ???
}
