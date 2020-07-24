package tradex.domain
package repository

import model.enums._

import model.instrument._

trait InstrumentRepository[M[_]] {
  /** query by account number */
  def query(isin: String): M[Option[Instrument]]

  /** query by instrument type Equity / FI / CCY */
  def queryByInstrumentType(instrumentType: InstrumentType): M[List[Instrument]]

  /** store */
  def store(ins: Instrument): M[Instrument]
}
