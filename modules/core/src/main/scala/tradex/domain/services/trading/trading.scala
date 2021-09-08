package tradex.domain
package services.trading

import java.time.LocalDate

import cats.data.NonEmptyList
import cats.syntax.all._

import NewtypeRefinedOps._
import effects.GenUUID
import model.account._
import model.execution._
import model.order.{ FrontOfficeOrder, Order, Quantity }
import model.trade._
import model.market.Market
import scala.util.control.NoStackTrace
import repository._

trait Trading[F[_]] {

  /** Find accounts opened on the date specified
    *
    * @param openDate the date when account was opened
    * @return a list of `Account` under the effect `F`
    */
  def getAccountsOpenedOn(openDate: LocalDate): F[List[Account]]

  /** Find the list of trades for the supplied client account no and (optionally)
    * the trade date.
    *
    * @param forAccountNo the client accountNo
    * @param forDate the trade date
    * @return a list of `Trade` under the effect `F`
    */
  def getTrades(
      forAccountNo: AccountNo,
      forDate: Option[LocalDate] = None
  ): F[List[Trade]]

  /** Create a list of `Order` from client orders read from a stream
    * as a csv file.
    *
    * @param csvOrder client order in csv format
    * @return a List of `Order` under the effect `F`
    */
  def orders(csvOrder: String): F[NonEmptyList[Order]]

  /** Create a list of `Order` from client orders that come from
    * the front office.
    *
    * @param frontOfficeOrders client order
    * @return a NonEmptyList of `Order` under the effect `F`
    */
  def orders(
      frontOfficeOrders: NonEmptyList[FrontOfficeOrder]
  ): F[NonEmptyList[Order]]

  /** Execute an `Order` in the `Market` and book the execution in the
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

  /** Allocate the `Execution` equally between the client accounts generating
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
  sealed trait TradingError extends NoStackTrace {
    def cause: String
  }
  case class OrderingError(cause: String)   extends TradingError
  case class ExecutionError(cause: String)  extends TradingError
  case class AllocationError(cause: String) extends TradingError

  def make[F[+_]: MonadThrowable: GenUUID](
      accountRepository: AccountRepository[F],
      executionRepository: ExecutionRepository[F],
      orderRepository: OrderRepository[F],
      tradeRepository: TradeRepository[F]
  ): Trading[F] =
    new Trading[F] {
      private final val ev = implicitly[MonadThrowable[F]]

      def getAccountsOpenedOn(openDate: LocalDate): F[List[Account]] =
        accountRepository.query(openDate)

      def getTrades(
          forAccountNo: AccountNo,
          forDate: Option[LocalDate] = None
      ): F[List[Trade]] =
        tradeRepository.query(
          forAccountNo,
          forDate.getOrElse(today.toLocalDate())
        )

      def orders(csvOrder: String): F[NonEmptyList[Order]] = {
        val action = ordering
          .createOrders(csvOrder)
          .fold(
            nec =>
              ev.raiseError(
                new Throwable(nec.toNonEmptyList.toList.mkString("/"))
              ),
            os => {
              if (os.isEmpty)
                ev.raiseError(
                  new Throwable("Empty order list received from csv")
                )
              else {
                val nlos = NonEmptyList.fromList(os).get
                persistOrders(nlos) *> ev.pure(nlos)
              }
            }
          )
        action.adaptError { case e =>
          OrderingError(Option(e.getMessage()).getOrElse("Unknown error"))
        }
      }

      def orders(
          frontOfficeOrders: NonEmptyList[FrontOfficeOrder]
      ): F[NonEmptyList[Order]] = {
        val action = Order
          .create(frontOfficeOrders)
          .fold(
            nec =>
              ev.raiseError(
                new Throwable(nec.toNonEmptyList.toList.mkString("/"))
              ),
            os => {
              if (os.isEmpty)
                ev.raiseError(
                  new Throwable("Empty order list received from csv")
                )
              else {
                val nlos = NonEmptyList.fromList(os).get
                persistOrders(nlos) *> ev.pure(nlos)
              }
            }
          )
        action.adaptError { case e =>
          OrderingError(Option(e.getMessage()).getOrElse("Unknown error"))
        }
      }

      def execute(
          orders: NonEmptyList[Order],
          market: Market,
          brokerAccountNo: AccountNo
      ): F[NonEmptyList[Execution]] = {

        val ois: NonEmptyList[(Order, model.order.LineItem)] = for {
          order <- orders
          item  <- order.items
        } yield (order, item)

        val exes: F[NonEmptyList[Execution]] = ois.traverse { case (order, item) =>
          Execution.execution(
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

        val action = exes.map(persistExecutions(_)) *> exes
        action.adaptError { case e =>
          ExecutionError(Option(e.getMessage()).getOrElse("Unknown error"))
        }
      }

      def allocate(
          executions: NonEmptyList[Execution],
          clientAccounts: NonEmptyList[AccountNo]
      ): F[NonEmptyList[Trade]] = {

        val anoExes: NonEmptyList[(AccountNo, Execution)] = for {
          execution <- executions
          accountNo <- clientAccounts
        } yield (accountNo, execution)

        val tradesNoTaxFee: F[NonEmptyList[Trade]] = anoExes.traverse { case (accountNo, execution) =>
          val q = execution.quantity.value.value / clientAccounts.size
          val qty = validate[Quantity](q)
            .fold(errs => throw new Exception(errs.toString), identity)

          Trade.trade(
            accountNo,
            execution.isin,
            execution.market,
            execution.buySell,
            execution.unitPrice,
            qty
          )
        }

        val trades = tradesNoTaxFee.map(_.map(Trade.withTaxFee))

        val action = trades.map(persistTrades(_)) *> trades
        action.adaptError { case e =>
          AllocationError(Option(e.getMessage()).getOrElse("Unknown error"))
        }
      }

      private def persistOrders(orders: NonEmptyList[Order]): F[Unit] =
        orderRepository.store(orders)

      private def persistExecutions(executions: NonEmptyList[Execution]): F[Unit] =
        executionRepository.store(executions)

      private def persistTrades(trades: NonEmptyList[Trade]): F[Unit] =
        tradeRepository.store(trades)
    }
}
