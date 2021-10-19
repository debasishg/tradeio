package tradex.domain
package repository
package interpreter
package memory

import java.time.LocalDate
import scala.collection.immutable.Map

import cats._
import cats.data.NonEmptyList
import cats.syntax.all._
import cats.effect._

import model.instrument.ISINCode
import model.trade._
import model.market._
import model.account._

// Constructor protected for the interpreter to prevent the Ref from leaking
// access through smart constructor below
class TradeRepositoryInterpreter[M[_]: Monad] protected (
    repo: Ref[M, Map[(String, ISINCode, TradeReferenceNo), Trade]]
) extends TradeRepository[M] {
  def query(accountNo: AccountNo, date: LocalDate): M[List[Trade]] =
    repo.get.map(
      _.values
        .filter(t => t.accountNo == accountNo && t.tradeDate.toLocalDate == date)
        .toList
    )

  def store(trd: Trade): M[Trade] =
    repo
      .update(_ + (((trd.accountNo.value.value, trd.isin, trd.refNo), trd)))
      .map(_ => trd)

  def store(trades: NonEmptyList[Trade]): M[Unit] =
    repo
      .update(
        _ ++ trades.toList
          .map(trd => (((trd.accountNo.value.value, trd.isin, trd.refNo), trd)))
      )
      .map(_ => ())

  def queryByMarket(market: Market): M[List[Trade]] = ???
  def all: M[List[Trade]]                           = ???
}

// Smart constructor
object TradeRepositoryInterpreter {
  def make[M[_]: Sync]: M[TradeRepositoryInterpreter[M]] =
    Ref
      .of[M, Map[(String, ISINCode, TradeReferenceNo), Trade]](Map.empty)
      .map(new TradeRepositoryInterpreter(_))
}
