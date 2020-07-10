package org.github.domain
package repository
package interpreter
package memory

import java.time.LocalDateTime
import scala.collection.immutable.Map 

import cats._
import cats.implicits._
import cats.effect.concurrent.Ref
import cats.effect.Sync

import model.trade._
import model.newtypes._

// Constructor private for the interpreter to prevent the Ref from leaking
// access through smart constructor below
final class TradeRepositoryInterpreter[M[_]: Monad] private (repo: Ref[M, Map[(AccountNo, ISINCode, TradeReferenceNo), Trade]])
  extends TradeRepository[M] {

  def query(accountNo: AccountNo, date: LocalDateTime): M[List[Trade]] = 
    repo.get.map(_.values.filter(t => t.accountNo == accountNo && t.tradeDate == date).toList)

  def store(trd: Trade): M[Trade] = 
    repo.update(_ + (((trd.accountNo, trd.isin, trd.refNo), trd))).map(_ => trd)
}

// Smart constructor 
object TradeRepositoryInterpreter {
  def make[M[_]: Sync]: M[TradeRepositoryInterpreter[M]] =
    Ref.of[M, Map[(AccountNo, ISINCode, TradeReferenceNo), Trade]](Map.empty).map(new TradeRepositoryInterpreter(_))
}