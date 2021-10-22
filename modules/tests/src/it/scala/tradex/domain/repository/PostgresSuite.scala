package tradex.domain
package repository

import cats.effect._
import cats.syntax.all._
import skunk._
import skunk.implicits._
import natchez.Trace.Implicits.noop
import generators._
import model.account._
import model.instrument._
import services.trading._
import services.accounting._

import suite.ResourceSuite
import java.time.LocalDate

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
          flushAll.traverse_(s.execute) >> s.execute(insertTaxFees).map(_ => ())
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

  test("Generate trades with input from front office") { postgres =>
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
}
