package tradex.domain
package repository
package interpreter
package memory

import scala.collection.immutable.Map

import cats._
import cats.implicits._
import cats.effect.concurrent.Ref
import cats.effect.Sync

import model.instrument._
import model.enums._

// Constructor private for the interpreter to prevent the Ref from leaking
// access through smart constructor below
final class InstrumentRepositoryInterpreter[M[_]: Monad] private (
    repo: Ref[M, Map[String, Instrument]]
) extends InstrumentRepository[M] {
  def query(isin: String): M[Option[Instrument]] = repo.get.map(_.get(isin))

  def queryByInstrumentType(
      instrumentType: InstrumentType
  ): M[List[Instrument]] =
    repo.get.map(_.values.filter(_.instrumentType == instrumentType).toList)

  def store(ins: Instrument): M[Instrument] =
    repo.update(_ + ((ins.isinCode.value.value, ins))).map(_ => ins)
}

// Smart constructor
object InstrumentRepositoryInterpreter {
  def make[M[_]: Sync]: M[InstrumentRepositoryInterpreter[M]] =
    Ref
      .of[M, Map[String, Instrument]](Map.empty)
      .map(new InstrumentRepositoryInterpreter(_))
}
