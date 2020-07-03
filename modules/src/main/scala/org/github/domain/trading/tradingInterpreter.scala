package org.github.domain
package trading

import java.util.UUID

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

  def orders(csvOrder: String): M[NonEmptyList[Order]] = {
    ordering
      .createOrders(csvOrder)
      .fold(
        nec => ev.raiseError(new Throwable(nec.toNonEmptyList.toList.mkString("/"))),
        os => ev.pure(NonEmptyList.fromList(os).get)
      )
  }

  def execute(orders: NonEmptyList[Order], market: Market, brokerAccountNo: AccountNo): M[NonEmptyList[Execution]] = ev.pure {
    orders.flatMap { order =>
      order.items.map { item =>
        Execution(
          ExecutionReferenceNo(UUID.randomUUID().toString),
          brokerAccountNo,
          order.no,
          item.instrument,
          market,
          item.buySell,
          item.unitPrice,
          item.quantity
        )
      }
    }
  }

  def allocate(executions: NonEmptyList[Execution], clientAccounts: NonEmptyList[AccountNo]): M[NonEmptyList[Trade]] = {
    executions.map{ execution =>
      val q = execution.quantity.value / clientAccounts.size
      clientAccounts.map { accountNo =>
        Trade.trade(
          accountNo, 
          execution.isin, 
          TradeReferenceNo(UUID.randomUUID().toString()), 
          execution.market, 
          execution.buySell,
          execution.unitPrice, 
          Quantity(q)
        )
      }.traverse(identity)
    }.traverse(identity)
    .fold(
      nec => ev.raiseError(new Throwable(nec.toNonEmptyList.toList.mkString("/"))),
      ls => ev.pure(ls.flatten)
    )
  }
}