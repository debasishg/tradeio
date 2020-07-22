package tradex.domain
package model

import enumeratum._

object enums {
  sealed abstract class BuySell(override val entryName: String)
      extends EnumEntry

  object BuySell extends Enum[BuySell] {
    case object Buy extends BuySell("buy")
    case object Sell extends BuySell("sell")

    val values = findValues
  }

  sealed trait InstrumentType extends EnumEntry

  object InstrumentType extends Enum[InstrumentType] {
    case object CCY extends InstrumentType
    case object Equity extends InstrumentType
    case object FixedIncome extends InstrumentType

    val values = findValues
  }

  sealed trait AccountType extends EnumEntry

  object AccountType extends Enum[AccountType] {
    case object Trading extends AccountType
    case object Settlement extends AccountType
    case object Both extends AccountType

    val values = findValues
  }
}
