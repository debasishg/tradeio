package org.github.domain
package trading

import cats._
import cats.data._
import cats.implicits._
import cats.effect._

import model.newtypes._
import model.market._
import model.trade._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val tradeInterpreterIO = new TradingInterpreter[IO]

    val csvOrder = ""
    val brokerAccountNo = AccountNo("b-123")
    val clientAccounts = NonEmptyList.of(AccountNo("a-123"), AccountNo("a-234"))

    for {
      trades <- program.tradeGeneration(tradeInterpreterIO, csvOrder, brokerAccountNo, Market.NewYork, clientAccounts)
      _ = trades.toList.foreach(println)
    } yield ExitCode.Success
  }
}

object program {
  def tradeGeneration[F[_]: FlatMap](
    trading: Trading[F], 
    csvOrder: String, 
    brokerAccountNo: AccountNo, 
    market: Market,
    clientAccountNos: NonEmptyList[AccountNo]): F[NonEmptyList[Trade]] = {

      for {
        orders       <- trading.orders(csvOrder)
        executions   <- trading.execute(orders, market, brokerAccountNo)
        trades       <- trading.allocate(executions, clientAccountNos)
      } yield trades
  }
}