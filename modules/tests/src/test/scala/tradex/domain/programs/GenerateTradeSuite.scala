package tradex.domain
package programs

import java.time.LocalDate
import cats.effect.IO
import cats.syntax.all._
import cats.data.NonEmptyList
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers
import model.account._
import model.trade._
import model.order._
import model.market._
import model.execution._
import model.balance._
import generators._
import services.trading.Trading
import services.accounting.Accounting
import repository.{ AccountRepository, BalanceRepository, ExecutionRepository, OrderRepository, TradeRepository }

object GenerateTradeSuite extends SimpleIOSuite with Checkers {
  val testAccountRepository   = new TestAccountRepository
  val testOrderRepository     = new TestOrderRepository
  val testExecutionRepository = new TestExecutionRepository
  val testTradeRepository     = new TestTradeRepository
  val testBalanceRepository   = new TestBalanceRepository

  val testTrading =
    Trading.make[IO](testAccountRepository, testExecutionRepository, testOrderRepository, testTradeRepository)

  val testAccounting = Accounting.make[IO](testBalanceRepository)

  val genTrade = programs.GenerateTrade[IO](testTrading, testAccounting)

  test("Successful generation of trades") {
    forall(generateTradeFrontOfficeInputGen) { frontOfficeInput =>
      genTrade.generate(frontOfficeInput).flatMap { case (trades, balances) =>
        expect.apply(trades.size > 0)
        expect.apply(balances.size > 0)
        expect.eql(trades.size, balances.size)
        IO(success)
      }
    }
  }
}

protected class TestAccountRepository extends AccountRepository[IO] {
  def query(no: AccountNo): IO[Option[Account]]                      = IO.pure(none[Account])
  def store(a: Account): IO[Account]                                 = IO(a)
  def query(openedOn: LocalDate): IO[List[Account]]                  = IO.pure(List.empty[Account])
  def all: IO[List[Account]]                                         = IO.pure(List.empty[Account])
  def allClosed(closeDate: Option[LocalDate]): IO[List[Account]]     = IO.pure(List.empty[Account])
  def allAccountsOfType(accountType: AccountType): IO[List[Account]] = IO.pure(List.empty[Account])
}

protected class TestExecutionRepository extends ExecutionRepository[IO] {
  def store(exe: Execution): IO[Execution]                 = IO(exe)
  def store(executions: NonEmptyList[Execution]): IO[Unit] = IO.pure(())
}

protected class TestOrderRepository extends OrderRepository[IO] {
  def query(no: OrderNo): IO[Option[Order]]              = IO.pure(none[Order])
  def queryByOrderDate(date: LocalDate): IO[List[Order]] = IO.pure(List.empty[Order])
  def store(ord: Order): IO[Order]                       = IO(ord)
  def store(orders: NonEmptyList[Order]): IO[Unit]       = IO.pure(())
}

protected class TestTradeRepository extends TradeRepository[IO] {
  def query(accountNo: AccountNo, date: LocalDate): IO[List[Trade]] = IO.pure(List.empty[Trade])
  def queryByMarket(market: Market): IO[List[Trade]]                = IO.pure(List.empty[Trade])
  def all: IO[List[Trade]]                                          = IO.pure(List.empty[Trade])
  def store(trd: Trade): IO[Trade]                                  = IO(trd)
  def store(trades: NonEmptyList[Trade]): IO[Unit]                  = IO.pure(())
}

protected class TestBalanceRepository extends BalanceRepository[IO] {
  def query(no: AccountNo): IO[Option[Balance]] = IO.pure(none[Balance])
  def store(a: Balance): IO[Balance]            = IO(a)
  def query(date: LocalDate): IO[List[Balance]] = IO.pure(List.empty[Balance])
  def all: IO[List[Balance]]                    = IO.pure(List.empty[Balance])
}
