package org.github.domain
package model

import io.estatico.newtype.macros.newtype

object newtypes {
  // account
  @newtype case class AccountNo(value: String)
  @newtype case class AccountName(value: String)
  
  // instrument
  @newtype case class ISINCode(value: String)
  @newtype case class InstrumentName(value: String)
  @newtype case class LotSize(value: Int)

  // order
  @newtype case class OrderNo(value: String)
  @newtype case class Quantity(value: BigDecimal)

  // execution
  @newtype case class ExecutionReferenceNo(value: String)
  @newtype case class UnitPrice(value: BigDecimal)

  // trade
  @newtype case class TradeReferenceNo(value: String)
}
