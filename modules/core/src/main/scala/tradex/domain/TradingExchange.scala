package tradex.domain

import cats.data.NonEmptyList
import cats.{ Order => _, _ }

import cats.syntax.all._
import cats.effect._
import cats.effect.{ Deferred, Ref }
import cats.effect.unsafe.implicits.global

import eu.timepit.refined.auto._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric._

import model.account.AccountNo
import model.instrument.ISINCode
import model.order._
import model.execution._
import model.trade._
import NewtypeRefinedOps._
import AppData._
import effects.GenUUID

object ExchangeApp extends IOApp.Simple {
  override def run: IO[Unit] = {
    val tradeGen = Exchange.create[IO]().flatMap { exchange =>
      import exchange._

      // `parTupled` gives a safe way to execute on fibers
      // it starts, joins fibers and handles cancelation
      // gracefully
      // once we have order and executions we can then start
      // executing allocations on another fiber. Here we use
      // `bracket` that handles cancellations
      (
        feedOrder(o1),
        feedExecutions(o1.no, NonEmptyList.fromListUnsafe(executions))
      ).parTupled *>
        allocate(o1.no, NonEmptyList.of(ano2, ano3)).start
          .bracket { _.join }(_.cancel)
    }

    tradeGen
      .flatMap { f =>
        f.fold(
          IO.println("Trade generation canceled") *> MonadCancel[IO].canceled,
          th => IO.raiseError(th),
          iots =>
            iots
              .flatMap { ts =>
                IO(ts.foreach(println))
              }
        )
      }
      .unsafeRunSync()

    IO.unit
  }
}

// FSM with the algebra of the commands
trait TradingExchange[F[_]] {
  // feed order into the system
  def feedOrder(order: Order): F[Unit]

  // feed execution, which will update order for fullfilment
  def feedExecutions(
      orderNo: OrderNo,
      executions: NonEmptyList[Execution]
  ): F[Unit]

  // allocate executions for this order and generate trade
  def allocate(
      orderNo: OrderNo,
      clientAccountNos: NonEmptyList[AccountNo]
  ): F[List[Trade]]
}

object Exchange {
  case class ApplicationState(
      order: Option[Order] = None,
      fulfilledOrder: Map[ISINCode, Quantity] = Map.empty,
      executions: List[Execution] = List.empty,
      clientAccountNos: List[AccountNo] = List.empty,
      trades: List[Trade] = List.empty
  ) {
    // check if all order items have been fulfilled by executions
    def orderFulfilled: Boolean = {
      val forder = fulfilledOrder.foldLeft(Map.empty[String, BigDecimal]) { (a, e) =>
        a ++ Map(e._1.value.value -> e._2.value.value)
      }
      order
        .map { ord =>
          val lis = ord.items
          val liMap = lis.foldLeft(Map.empty[String, BigDecimal]) { (a, li) =>
            Monoid.combineAll(
              List(a, Map(li.instrument.value.value -> li.quantity.value.value))
            )
          }
          val zero: BigDecimal Refined NonNegative = BigDecimal(0)
          liMap.foldLeft(true) { (a, e) =>
            if (
              forder
                .get(e._1)
                .getOrElse(zero) == e._2
            ) (a && true)
            else (a && false)
          }
        }
        .getOrElse(false)
    }
  }

  // create the FSM
  // the states of the FSM are abstracted within the creator
  def create[F[_]: Concurrent: GenUUID](): F[TradingExchange[F]] = {
    sealed trait State
    // application state has a value
    case class Value(s: ApplicationState) extends State
    // application state is being updated - need a `Deferred` for
    // synchronization
    case class Updating(d: Deferred[F, Either[Throwable, ApplicationState]]) extends State
    // application state has no value
    case object NoValue extends State

    Ref.of[F, State](NoValue).map { state =>
      new TradingExchange[F] {
        def feedOrder(ord: Order): F[Unit] = {
          Deferred[F, Either[Throwable, ApplicationState]].flatMap { newV =>
            state
              .modify {
                case NoValue =>
                  Updating(newV) -> addOrder(newV, ord, ApplicationState()).rethrow
                case Value(s) =>
                  Updating(newV) -> addOrder(newV, ord, s).rethrow
                case Updating(inFlight) =>
                  Updating(newV) ->
                    (for {
                      currAppState <- inFlight.get.rethrow
                      r            <- addOrder(newV, ord, currAppState)
                    } yield r).rethrow
              }
              .flatten
              .map(_ => ())
          }
        }

        private def addOrder(
            d: Deferred[F, Either[Throwable, ApplicationState]],
            order: Order,
            currentState: ApplicationState
        ): F[Either[Throwable, ApplicationState]] = {
          for {
            r <- updateStateWithOrderInfo(currentState, order).pure[F].attempt
            _ <- state.set {
              r match {
                case Left(_)  => NoValue
                case Right(v) => Value(v)
              }
            }
            _ <- d.complete(r)
          } yield r
        }

        private def updateStateWithOrderInfo(
            appState: ApplicationState,
            ord: Order
        ): ApplicationState = {
          // ignore input order if an order is already present
          appState.order
            .map(_ => appState)
            .getOrElse(appState.copy(order = Some(ord)))
        }

        def feedExecutions(
            orderNo: OrderNo,
            execs: NonEmptyList[Execution]
        ): F[Unit] = {
          Deferred[F, Either[Throwable, ApplicationState]].flatMap { newV =>
            state
              .modify {
                // switch to Updating and add execution info to state. Note we don't have
                // order in yet - still fulfillment info will be updated in anticipation
                // that we will have the order subsequently
                case NoValue =>
                  Updating(newV) -> addExecution(
                    newV,
                    orderNo,
                    execs,
                    ApplicationState()
                  ).rethrow

                // we have a current state - hence pass that while updating execution info
                case Value(s) =>
                  Updating(newV) -> addExecution(newV, orderNo, execs, s).rethrow

                // we are in Updating - hence need to wait for completion (`get` will wait)
                // once complete, update with execution info
                case Updating(inFlight) =>
                  Updating(newV) ->
                    (for {
                      currAppState <- inFlight.get.rethrow
                      r            <- addExecution(newV, orderNo, execs, currAppState)
                    } yield r).rethrow
              }
              .flatten
              .map(_ => ())
          }
        }

        private def addExecution(
            d: Deferred[F, Either[Throwable, ApplicationState]],
            orderNo: OrderNo,
            execs: NonEmptyList[Execution],
            currentState: ApplicationState
        ): F[Either[Throwable, ApplicationState]] =
          for {
            r <- updateStateWithExecutionInfo(currentState, orderNo, execs)
              .pure[F]
              .attempt
            _ <- state.set {
              r match {
                case Left(_)  => NoValue
                case Right(v) => Value(v)
              }
            }
            _ <- d.complete(r)
          } yield r

        private def updateStateWithExecutionInfo(
            appState: ApplicationState,
            orderNo: OrderNo,
            executions: NonEmptyList[Execution]
        ): ApplicationState = {
          val newFulfillmentMap =
            executions.foldLeft(appState.fulfilledOrder) { (a, e) =>
              val ins = e.isin
              val qty = e.quantity
              a.updatedWith(ins)(
                _.map(q => {
                  validate[Quantity](q.value.value + qty.value.value)
                    .fold(
                      errs => throw new Exception(errs.toString),
                      identity
                    )
                }).orElse(
                  validate[Quantity](qty.value.value)
                    .fold(
                      errs => throw new Exception(errs.toString),
                      q => Some(q)
                    )
                )
              )
            }
          appState.copy(
            executions = appState.executions ++ executions.toList
              .filter(_.orderNo == orderNo),
            fulfilledOrder = newFulfillmentMap
          )
        }

        def allocate(
            orderNo: OrderNo,
            clientAccountNos: NonEmptyList[AccountNo]
        ): F[List[Trade]] = {
          Deferred[F, Either[Throwable, ApplicationState]].flatMap { newV =>
            state
              .modify {
                // switch to Updating and try to generate trades and update state.
                // if the order is absent or has not yet been fulfilled we only update the state
                // with client accounts and don't generate trade
                case NoValue =>
                  Updating(newV) -> updateStateAndGenerateTrades(
                    newV,
                    clientAccountNos,
                    ApplicationState()
                  ).rethrow

                // we use the current value of the state and follow the trade generation
                // process (don't generate trade if order is absent or it has not yet
                // been fulfilled)
                case Value(s) =>
                  Updating(newV) -> updateStateAndGenerateTrades(
                    newV,
                    clientAccountNos,
                    s
                  ).rethrow

                // we are in Updating and hence need to wait till completion. Then follow the
                // usual trade generation process
                case Updating(inFlight) =>
                  Updating(newV) ->
                    (for {
                      currAppState <- inFlight.get.rethrow
                      r <- updateStateAndGenerateTrades(
                        newV,
                        clientAccountNos,
                        currAppState
                      )
                    } yield r).rethrow
              }
              .flatMap(_.map(_.trades))
          }
        }

        private def updateStateAndGenerateTrades(
            d: Deferred[F, Either[Throwable, ApplicationState]],
            accountNos: NonEmptyList[AccountNo],
            currentState: ApplicationState
        ) = {
          if (currentState.order.isDefined && currentState.orderFulfilled)
            // generate trade only if order has been received and fulfilled by executions
            generateTrades(d, accountNos, currentState)
          else
            // else just update the client accounts list
            updateStateWithClientAccounts(d, accountNos, currentState)
        }

        private def updateStateWithClientAccounts(
            d: Deferred[F, Either[Throwable, ApplicationState]],
            accountNos: NonEmptyList[AccountNo],
            currentState: ApplicationState
        ): F[Either[Throwable, ApplicationState]] =
          for {
            r <- currentState
              .copy(
                clientAccountNos = (currentState.clientAccountNos ++ accountNos.toList).distinct
              )
              .pure[F]
              .attempt

            _ <- state.set {
              r match {
                case Left(_)  => NoValue
                case Right(v) => Value(v)
              }
            }
            _ <- d.complete(r)
          } yield r

        private def generateTrades(
            d: Deferred[F, Either[Throwable, ApplicationState]],
            accountNos: NonEmptyList[AccountNo],
            currentState: ApplicationState
        ): F[Either[Throwable, ApplicationState]] =
          for {
            r <- allocate(
              NonEmptyList.fromList(currentState.executions).get,
              accountNos
            ).map(trds => currentState.copy(trades = trds.toList)).attempt

            _ <- state.set {
              r match {
                case Left(_)  => NoValue
                case Right(v) => Value(v)
              }
            }
            _ <- d.complete(r)
          } yield r

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

            Trade
              .trade[F](
                accountNo,
                execution.isin,
                execution.market,
                execution.buySell,
                execution.unitPrice,
                qty
              )
              .map(_.fold(errs => throw new Exception(errs.toString), identity))
          }

          tradesNoTaxFee.map(_.map(Trade.withTaxFee))
        }
      }
    }
  }
}
