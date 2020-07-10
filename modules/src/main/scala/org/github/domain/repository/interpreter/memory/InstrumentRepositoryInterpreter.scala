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

import common._
import model.instrument._
import model.newtypes._
import model.enums._

// Constructor private for the interpreter to prevent the Ref from leaking
// access through smart constructor below
final class InstrumentRepositoryInterpreter[M[_]: Monad] private (repo: Ref[M, Map[ISINCode, Instrument]])
  extends InstrumentRepository[M] {

  def query(isin: ISINCode): M[Option[Instrument]] = repo.get.map(_.get(isin))

  def queryByInstrumentType(instrumentType: InstrumentType): M[List[Instrument]] = 
    repo.get.map(_.values.filter(_.instrumentType == instrumentType).toList)

  def store(ins: Instrument): M[Instrument] = repo.update(_ + ((ins.isinCode, ins))).map(_ => ins)
}

// Smart constructor 
object InstrumentRepositoryInterpreter {
  def make[M[_]: Sync]: M[InstrumentRepositoryInterpreter[M]] =
    Ref.of[M, Map[ISINCode, Instrument]](Map.empty).map(new InstrumentRepositoryInterpreter(_))
}