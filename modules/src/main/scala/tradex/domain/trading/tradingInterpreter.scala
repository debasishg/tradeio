package tradex.domain
package trading

import java.util.UUID
import java.time.LocalDate

import cats.MonadError
import cats.data.NonEmptyList
import cats.implicits._
import cats.mtl._

import common._
import model.account.Account
import model.execution.Execution
import model.order.Order
import model.trade.Trade
import model.market.Market
import model.newtypes._

import repository._

class TradingInterpreter[M[_]: MonadThrowable](
    implicit A: ApplicativeAsk[M, AccountRepository[M]],
    E: ApplicativeAsk[M, ExecutionRepository[M]],
    I: ApplicativeAsk[M, InstrumentRepository[M]],
    O: ApplicativeAsk[M, OrderRepository[M]],
    T: ApplicativeAsk[M, TradeRepository[M]]
) extends Trading[M] {
  private final val ev = implicitly[MonadThrowable[M]]

  def getTrades(forAccountNo: AccountNo, forDate: Option[LocalDate] = None): M[List[Trade]] = 
    for {
      repo <- T.ask
      trades <- repo.query(forAccountNo, forDate.getOrElse(today.toLocalDate()))
    } yield (trades)

  def orders(csvOrder: String): M[NonEmptyList[Order]] = {
    ordering
      .createOrders(csvOrder)
      .fold(
        nec =>
          ev.raiseError(new Throwable(nec.toNonEmptyList.toList.mkString("/"))),
        os => {
          if (os isEmpty)
            ev.raiseError(new Throwable("Empty order list received from csv"))
          else {
            val nlos = NonEmptyList.fromList(os).get
            persistOrders(nlos) *> ev.pure(nlos)
          }
        }
      )
  }

  def execute(
      orders: NonEmptyList[Order],
      market: Market,
      brokerAccountNo: AccountNo
  ): M[NonEmptyList[Execution]] = {
    val exes = orders.flatMap { order =>
      order.items.map { item =>
        Execution(
          ExecutionReferenceNo(UUID.randomUUID().toString),
          brokerAccountNo,
          order.no,
          item.instrument,
          market,
          item.buySell,
          item.unitPrice,
          item.quantity,
          today
        )
      }
    }
    persistExecutions(exes) *> ev.pure(exes)
  }

  def allocate(
      executions: NonEmptyList[Execution],
      clientAccounts: NonEmptyList[AccountNo]
  ): M[NonEmptyList[Trade]] = {
    executions
      .map { execution =>
        val q = execution.quantity.value / clientAccounts.size
        clientAccounts
          .map { accountNo =>
            Trade.trade(
              accountNo,
              execution.isin,
              TradeReferenceNo(UUID.randomUUID().toString()),
              execution.market,
              execution.buySell,
              execution.unitPrice,
              Quantity(q)
            )
          }
          .traverse(identity)
      }
      .traverse(identity)
      .fold(
        nec =>
          ev.raiseError(new Throwable(nec.toNonEmptyList.toList.mkString("/"))),
        ls => {
          val trds = ls.flatten
          persistTrades(trds) *> ev.pure(trds)
        }
      )
  }

  private def persistOrders(orders: NonEmptyList[Order]) =
    for {
      repo <- O.ask
      _ <- repo.store(orders)
    } yield (())

  private def persistExecutions(executions: NonEmptyList[Execution]) =
    for {
      repo <- E.ask
      _ <- repo.store(executions)
    } yield (())

  private def persistTrades(trades: NonEmptyList[Trade]) =
    for {
      repo <- T.ask
      _ <- repo.store(trades)
    } yield (())
}
