package tradex.domain
package repository

import scala.concurrent.duration._
import cats.data.NonEmptyList
import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._
import skunk._
import skunk.implicits._
import natchez.Trace.Implicits.noop
import generators._
import model.account._
import model.instrument._
import model.trade._
import model.user._
import model.balance._
import services.trading._
import services.accounting._

import suite.ResourceSuite
import java.time.LocalDate
import org.scalacheck.Gen

object PostgresSuite extends ResourceSuite {

  val listTables =
    List("executions", "lineItems", "orders", "tradeTaxFees", "trades", "balance", "users", "instruments", "accounts")

  val listAll = listTables ++ List("taxfees")

  val flushAll: List[Command[Void]] =
    listAll.map { table =>
      sql"DELETE FROM #$table".command
    }

  val flushTables: List[Command[Void]] =
    listTables.map { table =>
      sql"DELETE FROM #$table".command
    }

  val insertTaxFees: Command[Void] =
    sql"""
      insert into taxFees (taxFeeId, description)
      values 
        ('TradeTax', 'Trade Tax'),
        ('Commission', 'Commission'),
        ('VAT', 'VAT'),
        ('Surcharge', 'Surcharge')""".command

  type Res = Resource[IO, Session[IO]]

  override def sharedResource: Resource[IO, Res] =
    Session
      .pooled[IO](
        host = "localhost",
        port = 5432,
        user = "postgres",
        password = None,
        database = "trading",
        max = 10,
        strategy = Strategy.SearchPath
      )
      .afterAll {
        _.use { s =>
          Temporal[IO].sleep(1.minute) >> flushAll.traverse_(s.execute) >> s.execute(insertTaxFees).map(_ => ())
        }
      }
      .beforeAll {
        _.use { s =>
          flushAll.traverse_(s.execute) >> s.execute(insertTaxFees).map(_ => ())
        }
      }
      .beforeEach {
        _.use { s =>
          flushTables.traverse_(s.execute)
        }
      }

  test("Accounts with no upsert") { postgres =>
    val a = AccountRepository.make[IO](postgres)
    forall(accountGen) { account =>
      for {
        x <- a.query(account.no)
        _ <- a.store(account, upsert = false)
        y <- a.all
        z <- a.store(account, upsert = false).attempt
      } yield expect.all(x.isEmpty, y.count(_.no === account.no) === 1, z.isLeft)
    }
  }

  test("Accounts with upsert") { postgres =>
    val a = AccountRepository.make[IO](postgres)
    forall(accountGen) { account =>
      for {
        x <- a.query(account.no)
        _ <- a.store(account)
        y <- a.all
        z <- a.store(account).attempt
      } yield expect.all(x.isEmpty, y.count(_.no === account.no) === 1, z.isRight)
    }
  }

  test("Instruments with upsert") { postgres =>
    val i = InstrumentRepository.make[IO](postgres)
    forall(equityGen) { equity =>
      for {
        x <- i.query(equity.isinCode)
        _ <- i.store(equity)
        y <- i.query(equity.isinCode)
        z <- i.store(equity).attempt
      } yield expect.all(x.isEmpty, y.count(_.isinCode === equity.isinCode) === 1, z.isRight)
    }
  }

  test("Generate trades with tax/fees") { postgres =>
    val a = AccountRepository.make[IO](postgres)
    val i = InstrumentRepository.make[IO](postgres)
    val t = TradeRepository.make[IO](postgres)

    for {
      _ <- forall(tradingAccountGen) { acc =>
        for {
          _       <- a.store(acc)
          fetched <- a.query(acc.no)
        } yield expect.all(fetched.isDefined, fetched.count(_.no === acc.no) === 1)
      }
      _ <- forall(equityGen) { ins =>
        for {
          _       <- i.store(ins)
          fetched <- i.query(ins.isinCode)
        } yield expect.all(fetched.isDefined, fetched.count(_.isinCode === ins.isinCode) === 1)
      }
      acc <- a.all
      ins <- i.queryByInstrumentType(InstrumentType.Equity)
      _ <- forall(tradeWithTaxFeeForAccountAndInstrumentGen(acc.head.no, ins.head.isinCode)) {
        _.flatMap { trd =>
          for {
            x <- t.all
            _ <- t.store(trd)
            y <- t.queryByMarket(trd.market)
            z <- t.query(trd.accountNo, LocalDate.now())
          } yield expect.all(
            x.isEmpty,
            y.count(_.market === trd.market) === 1,
            y.forall(_.netAmount.isDefined),
            z.count(_.accountNo === trd.accountNo) === 1
          )
        }
      }
    } yield expect.all(acc.nonEmpty, ins.nonEmpty)
  }

  test("Generation of trades with input from front office succeeds") { postgres =>
    val a = AccountRepository.make[IO](postgres)
    val i = InstrumentRepository.make[IO](postgres)
    val e = ExecutionRepository.make[IO](postgres)
    val o = OrderRepository.make[IO](postgres)
    val t = TradeRepository.make[IO](postgres)
    val b = BalanceRepository.make[IO](postgres)

    val accountsInstruments: IO[(List[AccountNo], List[ISINCode])] = for {
      _ <- forall(tradingAccountGen) { acc =>
        for {
          _       <- a.store(acc)
          fetched <- a.query(acc.no)
        } yield expect.all(fetched.isDefined, fetched.count(_.no === acc.no) === 1)
      }
      _ <- forall(equityGen) { ins =>
        for {
          _       <- i.store(ins)
          fetched <- i.query(ins.isinCode)
        } yield expect.all(fetched.isDefined, fetched.count(_.isinCode === ins.isinCode) === 1)
      }
      acc <- a.all
      ins <- i.queryByInstrumentType(InstrumentType.Equity)
    } yield (acc.map(_.no), ins.map(_.isinCode))

    val testTrading =
      Trading.make[IO](a, e, o, t)

    val testAccounting = Accounting.make[IO](b)

    val genTrade = programs.GenerateTrade[IO](testTrading, testAccounting)

    accountsInstruments.flatMap { ais =>
      val gen = for {
        u <- commonUserGen
        t <- generateTradeFrontOfficeInputGenWithAccountAndInstrument(ais._1, ais._2)
      } yield (u, t)

      forall(gen) { case (user, foTrades) =>
        genTrade
          .generate(foTrades, user.value.userId)
          .attempt
          .map {
            case Left(err: Trading.TradingError) => failure(s"Trade Generation Error: ${err.cause}")
            case Left(th: Throwable) => failure(th.getMessage())
            case Right((trades, balances)) => {
              expect.all(trades.size > 0, balances.size > 0)
              val totalTradedAmount = trades.toList.map(_.netAmount.get.amount).sum
              val totalBalanceChange = balances.toList.map(_.amount.amount).sum
              expect.eql(totalTradedAmount, totalBalanceChange)
              success
            }
          }
      }
    }
  }

  test("Generation of trades with input from front office fails") { postgres =>
    val a = AccountRepository.make[IO](postgres)
    val i = InstrumentRepository.make[IO](postgres)
    val e = ExecutionRepository.make[IO](postgres)
    val o = OrderRepository.make[IO](postgres)
    val t = TradeRepository.make[IO](postgres)
    val b = BalanceRepository.make[IO](postgres)

    val accountsInstruments: IO[(List[AccountNo], List[ISINCode])] = for {
      _ <- forall(tradingAccountGen) { acc =>
        for {
          _       <- a.store(acc)
          fetched <- a.query(acc.no)
        } yield expect.all(fetched.isDefined, fetched.count(_.no === acc.no) === 1)
      }
      _ <- forall(equityGen) { ins =>
        for {
          _       <- i.store(ins)
          fetched <- i.query(ins.isinCode)
        } yield expect.all(fetched.isDefined, fetched.count(_.isinCode === ins.isinCode) === 1)
      }
      acc <- a.all
      ins <- i.queryByInstrumentType(InstrumentType.Equity)
    } yield (acc.map(_.no), ins.map(_.isinCode))

    val testTrading =
      Trading.make[IO](a, e, o, t)

    val testAccounting = Accounting.make[IO](b)

    val genTrade = programs.GenerateTrade[IO](testTrading, testAccounting)

    accountsInstruments.flatMap { ais =>
      val gen = for {
        u <- commonUserGen
        t <- generateTradeFrontOfficeInputGenWithAccountAndInstrument(ais._1, ais._2)
      } yield (u, t)

      forall(gen) { case (user, foTrades) => {
        val invalidInput = foTrades.copy(
          frontOfficeOrders =
            foTrades.frontOfficeOrders.map(forder => forder.copy(isin = "123", qty = BigDecimal.valueOf(-10)))
        )
        genTrade
          .generate(invalidInput, user.value.userId)
          .attempt
          .map {
            case Left(err: Trading.TradingError) => 
              expect.all(err.cause.contains("Quantity has to be positive"))
              success
            case Left(th: Throwable) => failure(th.getMessage())
            case Right(_) => failure("Trade generation must fail owing to invalid input")
          }
        }
      }
    }
  }

  test("Concurrent generation of trades with input from front office succeeds") { postgres =>
    val a = AccountRepository.make[IO](postgres)
    val i = InstrumentRepository.make[IO](postgres)
    val e = ExecutionRepository.make[IO](postgres)
    val o = OrderRepository.make[IO](postgres)
    val t = TradeRepository.make[IO](postgres)
    val b = BalanceRepository.make[IO](postgres)

    val accountsInstruments: IO[(List[AccountNo], List[ISINCode])] = for {
      _ <- forall(tradingAccountGen) { acc =>
        for {
          _       <- a.store(acc)
          fetched <- a.query(acc.no)
        } yield expect.all(fetched.isDefined, fetched.count(_.no === acc.no) === 1)
      }
      _ <- forall(equityGen) { ins =>
        for {
          _       <- i.store(ins)
          fetched <- i.query(ins.isinCode)
        } yield expect.all(fetched.isDefined, fetched.count(_.isinCode === ins.isinCode) === 1)
      }
      acc <- a.all
      ins <- i.queryByInstrumentType(InstrumentType.Equity)
    } yield (acc.map(_.no), ins.map(_.isinCode))

    val testTrading =
      Trading.make[IO](a, e, o, t)

    val testAccounting = Accounting.make[IO](b)

    val genTrade = programs.GenerateTrade[IO](testTrading, testAccounting)
    val console = IO.consoleForIO

    // the producer can exit when the list is exhausted
    def producer(
        id: Int,
        foTradesR: Ref[IO, List[GenerateTradeFrontOfficeInput]],
        queue: Queue[IO, GenerateTradeFrontOfficeInput]
    ): IO[Unit] =
      for {
        i <- foTradesR.modify { l => 
          if (l.isEmpty) (l, None)
          else (l.tail, Some(l.head))
        }
        _ <- i.map(e => queue.offer(e)).getOrElse(IO.unit)
        _ <- i.map(_ => producer(id, foTradesR, queue)).getOrElse(IO.unit)
      } yield ()
  
    // using `tryTake` instead of `take` since not getting an element means
    // the producer has exhausted the list it was handed to and the concumer
    // can exit
    def consumer(id: Int, queue: Queue[IO, GenerateTradeFrontOfficeInput], userId: UserId): IO[Unit] =
      for {
        i <- queue.tryTake
        _ <- i.map(e => generateTrade(e, userId)).getOrElse(IO.unit)
        _ <- i.map(_ => consumer(id, queue, userId)).getOrElse(IO.unit)
      } yield ()

    def generateTrade(fi: GenerateTradeFrontOfficeInput, 
      userId: UserId): IO[(NonEmptyList[Trade], NonEmptyList[Balance])] = {
      genTrade.generate(fi, userId)
    } 

    accountsInstruments.flatMap { ais =>
      val gen = for {
        u <- commonUserGen
        t <- Gen.listOfN(4, generateTradeFrontOfficeInputGenWithAccountAndInstrument(ais._1, ais._2))
      } yield (u, t)

      // 2 producers and 2 consumers per list of `GenerateTradeFrontOfficeInput`
      // generated
      forall(gen) { case (user, foTrades) =>
        for {
          queue     <- Queue.bounded[IO, GenerateTradeFrontOfficeInput](100)
          foTradesR <- Ref.of[IO, List[GenerateTradeFrontOfficeInput]](foTrades)
          // can use 1 producer / consumer as well
          // p = producer(1, foTradesR, queue) 
          // c = consumer(1, queue, user.value.userId)
          // _ <- (p, c).parTupled
          producers = List.range(1, 3).map(producer(_, foTradesR, queue))            // 2 producers
          consumers = List.range(1, 3).map(consumer(_, queue, user.value.userId))    // 2 consumers
          _ <- (producers ++ consumers).parSequence
            .as(
              ExitCode.Success
            ) 
            .handleErrorWith { t =>
              console.errorln(s"Error caught: ${t.getMessage}").as(ExitCode.Error)
            }
        } yield success
      }
    }
  }
}
