package org.github.domain
package trading

import java.time.Instant

import cats._
import cats.data._
import cats.implicits._
import cats.effect._

import io.chrisdavenport.cormorant._
import io.chrisdavenport.cormorant.generic.semiauto._
import io.chrisdavenport.cormorant.implicits._

import model.newtypes._
import model.market._
import model.trade._
import model.order._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val tradeInterpreterIO = new TradingInterpreter[IO]

    val csvOrder = orderGenerator.generateOrders()
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

object orderGenerator {
    def generateOrders(): String = {
      val o1 =
        FrontOfficeOrder("a-1", Instant.now(), "isin-12345", 100.00, 12.25, "B")
      val o2 =
        FrontOfficeOrder("a-1", Instant.now(), "isin-12346", 200.00, 22.25, "S")
      val o3 =
        FrontOfficeOrder("a-2", Instant.now(), "isin-12345", 100.00, 52.25, "B")

      val orders = List(o1, o2, o3)
      implicit val lw: LabelledWrite[FrontOfficeOrder] = deriveLabelledWrite
      orders.writeComplete.print(Printer.default)
    }
}