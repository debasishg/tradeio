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

import repository._
import interpreter.memory._

import Implicits._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {

    val csvOrder = orderGenerator.generateOrders()
    val brokerAccountNo = AccountNo("b-123")
    val clientAccounts = NonEmptyList.of(AccountNo("a-123"), AccountNo("a-234"))

    val trades = Algebras.make[IO].flatMap { algebras =>
      implicit val accountRepositoryAsk = DefaultApplicativeAsk.constant[IO, AccountRepository[IO]](algebras.accountRepository)
      implicit val instrumentRepositoryAsk = DefaultApplicativeAsk.constant[IO, InstrumentRepository[IO]](algebras.instrumentRepository)
      implicit val orderRepositoryAsk = DefaultApplicativeAsk.constant[IO, OrderRepository[IO]](algebras.orderRepository)
      implicit val tradeRepositoryAsk = DefaultApplicativeAsk.constant[IO, TradeRepository[IO]](algebras.tradeRepository)

      program.tradeGeneration(
        new TradingInterpreter[IO],
        csvOrder,
        brokerAccountNo,
        Market.NewYork,
        clientAccounts
      )
    }
    trades.unsafeRunSync.toList.foreach(println)
    IO(ExitCode.Success)
  }
}

object program {
  def tradeGeneration[F[_]: FlatMap](
      trading: Trading[F],
      csvOrder: String,
      brokerAccountNo: AccountNo,
      market: Market,
      clientAccountNos: NonEmptyList[AccountNo]
  ): F[NonEmptyList[Trade]] = {
    import trading._
    for {
      orders <- orders(csvOrder)
      executions <- execute(orders, market, brokerAccountNo)
      trades <- allocate(executions, clientAccountNos)
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
