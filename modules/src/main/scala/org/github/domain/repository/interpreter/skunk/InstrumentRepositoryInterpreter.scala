package org.github.domain
package repository
package interpreter.skunk

import java.time.LocalDateTime

import cats.data.NonEmptyList
import cats.implicits._
import cats.effect._
import cats.effect.concurrent.Ref

import skunk._
import skunk.data.Type
import skunk.codec.all._
import skunk.implicits._

import squants.market._
import squants.market.defaultMoneyContext

import common._
import model.newtypes._
import model.enums._
import model.instrument._
import Instrument._
import ext.skunkx._

final class InstrumentRepositoryInterpreter[M[_]: Sync] private (
  sessionPool: Resource[M, Session[M]]) extends InstrumentRepository[M] {
  def query(isin: ISINCode): M[Option[Instrument]] = ???
  def queryByInstrumentType(instrumentType: InstrumentType): M[List[Instrument]] = ???
  def store(ins: Instrument): M[Instrument] = ???
}
  
private object InstrumentQueries {

  // A codec that maps Postgres type `accountType` to Scala type `AccountType`
  val instrumentType = enum(InstrumentType, Type("instrumenttype"))
  implicit val moneyContext = defaultMoneyContext

  val decoder: Decoder[Instrument] =
    (varchar ~ varchar ~ instrumentType ~ timestamp.opt ~ timestamp.opt ~ int2.opt ~ numeric.opt ~ numeric.opt ~ numeric.opt).map {
      case isin ~ nm ~ tp ~ di ~ dm ~ ls ~ up ~ cr ~ cf =>
        Instrument(
          ISINCode(isin),
          InstrumentName(nm),
          tp,
          di,
          dm,
          LotSize(ls.getOrElse(0)),
          up.map(Money(_)),
          cr.map(Money(_)),
          cf
        )
    }

  val selectByISINCode: Query[ISINCode, Instrument] =
    sql"""
        SELECT i.isinCode, i.name, i.type, i.dateOfIssue, i.dateOfMaturity, i.lotSize, i.unitPrice, i.couponRate, i.couponFrequency
        FROM instruments AS i
        WHERE i.isinCode = ${varchar.cimap[ISINCode]}
       """.query(decoder)

  val selectByInstrumentType: Query[InstrumentType, Instrument] =
    sql"""
        SELECT i.isinCode, i.name, i.type, i.dateOfIssue, i.dateOfMaturity, i.lotSize, i.unitPrice, i.couponRate, i.couponFrequency
        FROM instruments AS i
        WHERE i.type = $instrumentType
       """.query(decoder)
}
/*
CREATE TABLE IF NOT EXISTS instruments (
    isinCode varchar NOT NULL PRIMARY KEY,
    name varchar NOT NULL,
    type instrumentType NOT NULL,
    dateOfIssue timestamp,
    dateOfMaturity timestamp,
    lotSize smallint,
    unitPrice decimal,
    couponRate decimal,
    couponFrequency decimal
)
*/