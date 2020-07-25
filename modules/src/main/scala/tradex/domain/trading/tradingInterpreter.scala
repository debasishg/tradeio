package tradex.domain
package trading

import java.time.LocalDate

import cats.data.NonEmptyList
import cats.implicits._
import cats.mtl._

import common._
import NewtypeRefinedOps._
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

  def getAccountsOpenedOn(openDate: LocalDate): M[List[Account]] =
    for {
      repo <- A.ask
      accounts <- repo.query(openDate)
    } yield accounts

  def getTrades(
      forAccountNo: String,
      forDate: Option[LocalDate] = None
  ): M[List[Trade]] =
    for {
      repo <- T.ask
      trades <- repo.query(forAccountNo, forDate.getOrElse(today.toLocalDate()))
    } yield trades

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
          Execution.generateExecutionReferenceNo(),
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
    val trades: NonEmptyList[Trade] = executions
      .flatMap { execution =>
        val q = execution.quantity.value.value / clientAccounts.size
        val qty = validate[Quantity](q)
          .fold(errs => throw new Exception(errs.toString), identity)
        clientAccounts
          .map { accountNo =>
            val trd = Trade(
              accountNo,
              execution.isin,
              Trade.generateTradeReferenceNo(),
              execution.market,
              execution.buySell,
              execution.unitPrice,
              qty
            )
            Trade.withTaxFee(trd)
          }
      }
    persistTrades(trades) *> ev.pure(trades)
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
