package org.github.domain
package trading

import java.util.UUID

import cats.{Order => OrderC, _}
import cats.data.NonEmptyList
import cats.implicits._
import cats.effect._
import cats.effect.concurrent.{Deferred, Ref}

import model.order._
import model.execution._
import model.trade._
import model.newtypes._
import common._
import AppData._

object ExchangeApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      s <- Exchange.create[IO]
      _ <- s.feedOrder(o1)
      _ <- s.feedExecution(o1.no, NonEmptyList.of(e1, e2))
      _ <- s.feedExecution(o1.no, NonEmptyList.of(e3, e4))
      trades <- s.allocate(o1.no, NonEmptyList.of(ano2, ano3))

      _ = trades.foreach(println)
    } yield ExitCode.Success
  }
}

trait TradingExchange[F[_]] {
  // feed order into the system
  def feedOrder(order: Order): F[Unit]

  // feed execution, which will update order for fullfilment
  def feedExecution(
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
      order
        .map { ord =>
          val lis = ord.items
          lis.foldLeft(true) { (a, li) =>
            val ins = li.instrument
            val qty = li.quantity
            if (fulfilledOrder.get(ins).getOrElse(Quantity(0)) == qty) a
            else false
          }
        }
        .getOrElse(false)
    }
  }

  def create[F[_]: Concurrent](): F[TradingExchange[F]] = {
    sealed trait State
    case class Value(s: ApplicationState) extends State
    case class Updating(d: Deferred[F, Either[Throwable, ApplicationState]])
        extends State
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
                case st @ Updating(inFlight) =>
                  Updating(newV) ->
                    (for {
                      currAppState <- inFlight.get.rethrow
                      r <- addOrder(newV, ord, currAppState)
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
                case Left(_) => NoValue
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
          appState.order
            .map(_ => appState)
            .getOrElse(appState.copy(order = Some(ord)))
        }

        def feedExecution(
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
                case st @ Updating(inFlight) =>
                  Updating(newV) ->
                    (for {
                      currAppState <- inFlight.get.rethrow
                      r <- addExecution(newV, orderNo, execs, currAppState)
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
        ) =
          for {
            r <- updateStateWithExecutionInfo(currentState, orderNo, execs)
              .pure[F]
              .attempt
            _ <- state.set {
              r match {
                case Left(_) => NoValue
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
          val newFulfillmentMap = executions.foldLeft(appState.fulfilledOrder) {
            (a, e) =>
              val ins = e.isin
              val qty = e.quantity
              a.updatedWith(ins)(
                _.map(q => Quantity(q.value + qty.value)).orElse(qty.some)
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
                    orderNo,
                    clientAccountNos,
                    ApplicationState()
                  ).rethrow

                // we use the current value of the state and follow the trade generation
                // process (don't generate trade if order is absent or it has not yet
                // been fulfilled)
                case Value(s) =>
                  Updating(newV) -> updateStateAndGenerateTrades(
                    newV,
                    orderNo,
                    clientAccountNos,
                    s
                  ).rethrow

                // we are in Updating and hence need to wait till completion. Then follow the
                // usual trade generation process
                case st @ Updating(inFlight) =>
                  Updating(newV) ->
                    (for {
                      currAppState <- inFlight.get.rethrow
                      r <- updateStateAndGenerateTrades(
                        newV,
                        orderNo,
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
            orderNo: OrderNo,
            accountNos: NonEmptyList[AccountNo],
            currentState: ApplicationState
        ) = {
          if (currentState.order.isDefined && currentState.orderFulfilled)
            // generate trade only if order has been received and fulfilled by executions
            generateTrades(d, orderNo, accountNos, currentState)
          else
            // else just update the client accounts list
            updateStateWithClientAccounts(d, accountNos, currentState)
        }

        private def updateStateWithClientAccounts(
            d: Deferred[F, Either[Throwable, ApplicationState]],
            accountNos: NonEmptyList[AccountNo],
            currentState: ApplicationState
        ) =
          for {
            r <- currentState
              .copy(
                clientAccountNos =
                  (currentState.clientAccountNos ++ accountNos.toList).distinct
              )
              .pure[F]
              .attempt

            _ <- state.set {
              r match {
                case Left(_) => NoValue
                case Right(v) => Value(v)
              }
            }
            _ <- d.complete(r)
          } yield r

        private def generateTrades(
            d: Deferred[F, Either[Throwable, ApplicationState]],
            orderNo: OrderNo,
            accountNos: NonEmptyList[AccountNo],
            currentState: ApplicationState
        ) =
          for {
            r <- currentState
              .copy(
                trades = allocate(
                  NonEmptyList.fromList(currentState.executions).get,
                  accountNos
                ).toList
              )
              .pure[F]
              .attempt

            _ <- state.set {
              r match {
                case Left(_) => NoValue
                case Right(v) => Value(v)
              }
            }
            _ <- d.complete(r)
          } yield r

        private def allocate(
            executions: NonEmptyList[Execution],
            clientAccounts: NonEmptyList[AccountNo]
        ): NonEmptyList[Trade] = {
          executions.flatMap { execution =>
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
              .toOption
              .get
          }
        }
      }
    }
  }
}
