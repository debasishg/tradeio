package tradex.domain
package repository
package interpreter.skunk

import cats.implicits._
import cats.effect._

import skunk._
import skunk.data.Type
import skunk.codec.all._
import skunk.implicits._

import squants.market._

import common._
import model.enums._
import model.instrument._

final class InstrumentRepositoryInterpreter[M[_]: Sync] private (
    sessionPool: Resource[M, Session[M]]
) extends InstrumentRepository[M] {
  import InstrumentQueries._

  def query(isin: String): M[Option[Instrument]] =
    sessionPool.use { session =>
      session.prepare(selectByISINCode).use { ps =>
        ps.option(isin)
      }
    }

  def queryByInstrumentType(
      instrumentType: InstrumentType
  ): M[List[Instrument]] =
    sessionPool.use { session =>
      session.prepare(selectByInstrumentType).use { ps =>
        ps.stream(instrumentType, 1024).compile.toList
      }
    }

  def store(ins: Instrument): M[Instrument] =
    sessionPool.use { session =>
      session.prepare(upsertInstrument).use { cmd =>
        cmd.execute(ins).void.map(_ => ins)
      }
    }
}

private object InstrumentQueries {
  val instrumentType = enum(InstrumentType, Type("instrumenttype"))

  val decoder: Decoder[Instrument] =
    (varchar ~ varchar ~ instrumentType ~ timestamp.opt ~ timestamp.opt ~ int2.opt ~ numeric.opt ~ numeric.opt ~ numeric.opt)
      .map {
        case isin ~ nm ~ tp ~ di ~ dm ~ ls ~ up ~ cr ~ cf =>
          Instrument
            .instrument(
              isin,
              nm,
              tp,
              di,
              dm,
              ls,
              up.map(Money(_)),
              cr.map(Money(_)),
              cf
            )
            .fold(errs => throw new Exception(errs.toString), identity)
      }

  val selectByISINCode: Query[String, Instrument] =
    sql"""
        SELECT i.isinCode, i.name, i.type, i.dateOfIssue, i.dateOfMaturity, i.lotSize, i.unitPrice, i.couponRate, i.couponFrequency
        FROM instruments AS i
        WHERE i.isinCode = $varchar
       """.query(decoder)

  val selectByInstrumentType: Query[InstrumentType, Instrument] =
    sql"""
        SELECT i.isinCode, i.name, i.type, i.dateOfIssue, i.dateOfMaturity, i.lotSize, i.unitPrice, i.couponRate, i.couponFrequency
        FROM instruments AS i
        WHERE i.type = $instrumentType
       """.query(decoder)

  val upsertInstrument: Command[Instrument] =
    sql"""
        INSERT INTO instruments
        VALUES ($varchar, $varchar, $instrumentType, ${timestamp.opt}, ${timestamp.opt}, ${int2.opt}, ${numeric.opt}, ${numeric.opt}, ${numeric.opt})
        ON CONFLICT(isinCode) DO UPDATE SET
          name                 = EXCLUDED.name,
          type                 = EXCLUDED.type,
          dateOfIssue          = EXCLUDED.dateOfIssue,
          dateOfMaturity       = EXCLUDED.dateOfMaturity,
          lotSize              = EXCLUDED.lotSize,
          unitPrice            = EXCLUDED.unitPrice,
          couponRate           = EXCLUDED.couponRate,
          couponFrequency      = EXCLUDED.couponFrequency
       """.command.contramap {
      case i =>
        i match {
          case Instrument(
              isinCode,
              name,
              typ,
              dateOfIssue,
              dateOfMaturity,
              lotSize,
              unitPrice,
              couponRate,
              couponFrequency
              ) =>
            isinCode.value.value ~ name.value.value ~ typ ~ dateOfIssue ~ dateOfMaturity ~ Option(
              lotSize.value.value
            ) ~ unitPrice.map(u => BigDecimal.decimal(u.value)) ~ couponRate
              .map(c => BigDecimal.decimal(c.value)) ~ couponFrequency
        }
    }
}

// Smart constructor
object InstrumentRepositoryInterpreter {
  def make[M[_]: Sync](
      sessionPool: Resource[M, Session[M]]
  ): M[InstrumentRepositoryInterpreter[M]] =
    Sync[M].delay(new InstrumentRepositoryInterpreter[M](sessionPool))
}
