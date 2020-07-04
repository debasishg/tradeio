package org.github.domain
package trading

import java.util.UUID

import cats.{ Order => OrderC, _ }
import cats.data.NonEmptyList
import cats.implicits._
import cats.effect._
import cats.effect.concurrent.{Deferred, Ref}

import model.order._
import model.execution._
import model.trade._
import model.newtypes._
import common._

object MainP extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val state = IO(Exchange.ApplicationState())
    for {

      s <- Exchange.create[IO](state)
      _ <- s.feedOrder(null)
      _ <- s.feedExecution(OrderNo("o1"), NonEmptyList.one(null))
      t <- s.allocate(OrderNo("o1"), NonEmptyList.one(AccountNo("a1")))
      _ = println(t)

    } yield ExitCode.Success
  }
}

trait TradingExchange[F[_]] {
  // feed order into the system
  def feedOrder(order: Order): F[Unit]

  // feed execution, which will update order for fullfilment
  def feedExecution(orderNo: OrderNo, executions: NonEmptyList[Execution]): F[Unit] 

  // allocate executions for this order and generate trade
  def allocate(orderNo: OrderNo, clientAccountNos: NonEmptyList[AccountNo]): F[List[Trade]]
}

object Exchange {
  case class ApplicationState(
    order: Option[Order] = None,
    // quantity so far fulfilled for this ISIN
    fulfilledOrder: Map[ISINCode, Quantity] = Map.empty,
    executions: List[Execution] = List.empty,
    trades: List[Trade] = List.empty
  )

  def create[F[_]: Concurrent](fa: F[ApplicationState]): F[TradingExchange[F]] = {

    sealed trait State
    case class Value(s: ApplicationState) extends State
    case class Updating(d: Deferred[F, Either[Throwable, ApplicationState]]) extends State
    case object NoValue extends State

    Ref.of[F, State](NoValue).map { state =>
      new TradingExchange[F] {

        def feedOrder(ord: Order): F[Unit] = state.update {
          case NoValue => Value(ApplicationState(Some(ord)))
          case Value(s) => Value(s)
          case st @ Updating(_) => st
        }

        def feedExecution(orderNo: OrderNo, execs: NonEmptyList[Execution]): F[Unit] = 
          Deferred[F, Either[Throwable, ApplicationState]].flatMap { newV =>
            state.modify {
              // switch to Updating and add execution info to state. Note we don't have
              // order in yet - still fulfillment info will be updated in anticipation
              // that we will have the order subsequently
              case NoValue => Updating(newV) -> addExecution(newV, orderNo, execs, ApplicationState()).rethrow

              // we have a current state - hence pass that while updating execution info
              case Value(s) => Updating(newV) -> addExecution(newV, orderNo, execs, s).rethrow

              // we are in Updating - hence need to wait for completion (`get` will wait)
              // once complete, update with execution info
              case st @ Updating(inFlight) => 
                Updating(newV) -> 
                  (for {
                    currAppState <- inFlight.get.rethrow  
                    r <- addExecution(newV, orderNo, execs, currAppState)
                  } yield r).rethrow
            }.map(_ => ())
          }

        private def addExecution(
          d: Deferred[F, Either[Throwable, ApplicationState]], 
          orderNo: OrderNo, 
          execs: NonEmptyList[Execution],
          currentState: ApplicationState) =
          for {
            r <- updateStateWithExecutionInfo(currentState, orderNo, execs).pure[F].attempt
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
          executions: NonEmptyList[Execution]): ApplicationState = {

          val newFulfillmentMap = executions.foldLeft(appState.fulfilledOrder) { (a, e) =>
            val ins = e.isin
            val qty = e.quantity
            a.updatedWith(ins)(_.map(q => Quantity(q.value + qty.value)))
          }
          appState.copy(
            executions = appState.executions ++ executions.toList.filter(_.orderNo == orderNo), 
            fulfilledOrder = newFulfillmentMap
          )
        }

        def allocate(orderNo: OrderNo, clientAccountNos: NonEmptyList[AccountNo]): F[List[Trade]] = 
          Deferred[F, Either[Throwable, ApplicationState]].flatMap { newV =>
            state.modify {
              case NoValue => Updating(newV) -> generateTrades(newV, orderNo, clientAccountNos, ApplicationState()).rethrow
              case Value(s) => Updating(newV) -> generateTrades(newV, orderNo, clientAccountNos, s).rethrow
              case st @ Updating(inFlight) => 
                Updating(newV) -> 
                  (for {
                    currAppState <- inFlight.get.rethrow  
                    r <- generateTrades(newV, orderNo, clientAccountNos, currAppState)
                  } yield r).rethrow
            }.flatMap(x => x.map(_.trades))
          }

        private def generateTrades(
          d: Deferred[F, Either[Throwable, ApplicationState]], 
          orderNo: OrderNo, 
          accountNos: NonEmptyList[AccountNo],
          currentState: ApplicationState) =
          for {
            r <- currentState.copy(trades = allocate(NonEmptyList.fromList(currentState.executions).get, accountNos).toList).pure[F].attempt
            _ <- state.set {
              r match {
                case Left(_) => NoValue
                case Right(v) => Value(v)
              }
            }
            _ <- d.complete(r)
          } yield r

        private def allocate(executions: NonEmptyList[Execution], clientAccounts: NonEmptyList[AccountNo]): NonEmptyList[Trade] = {
          executions.flatMap{ execution =>
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
            }.traverse(identity).toOption.get
          } 
        }
      }
    }
  }
}
