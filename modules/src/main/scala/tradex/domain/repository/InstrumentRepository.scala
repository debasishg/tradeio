package tradex.domain
package repository

import cats.syntax.all._
import cats.effect._

import skunk._
import skunk.data.Type
import skunk.codec.all._
import skunk.implicits._

import squants.market._

import model.instrument._
import codecs._

trait InstrumentRepository[F[_]] {
  /** query by account number */
  def query(isin: ISINCode): F[Option[Instrument]]

  /** query by instrument type Equity / FI / CCY */
  def queryByInstrumentType(instrumentType: InstrumentType): F[List[Instrument]]

  /** store */
  def store(ins: Instrument): F[Instrument]
}

object InstrumentRepository {
  def make[F[_]: Concurrent](
      postgres: Resource[F, Session[F]]
  ): InstrumentRepository[F] =
    new InstrumentRepository[F] {
      import InstrumentRepositorySQL._

      def query(isin: ISINCode): F[Option[Instrument]] =
        postgres.use { session =>
          session.prepare(selectByISINCode).use { ps =>
            ps.option(isin)
          }
        }

      def queryByInstrumentType(
          instrumentType: InstrumentType
      ): F[List[Instrument]] =
        postgres.use { session =>
          session.prepare(selectByInstrumentType).use { ps =>
            ps.stream(instrumentType, 1024).compile.toList
          }
        }

      def store(ins: Instrument): F[Instrument] =
        postgres.use { session =>
          session.prepare(upsertInstrument).use { cmd =>
            cmd.execute(ins).void.map(_ => ins)
          }
        }
    }
}

private object InstrumentRepositorySQL {
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

  val selectByISINCode: Query[ISINCode, Instrument] =
    sql"""
        SELECT i.isinCode, i.name, i.type, i.dateOfIssue, i.dateOfMaturity, i.lotSize, i.unitPrice, i.couponRate, i.couponFrequency
        FROM instruments AS i
        WHERE i.isinCode = $isinCode
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
