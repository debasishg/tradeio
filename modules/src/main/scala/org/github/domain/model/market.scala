package org.github.domain
package model

import enumeratum._
import enumeratum.EnumEntry._

object market {
  sealed abstract class Market(override val entryName: String) extends EnumEntry

  object Market extends Enum[Market] {
    case object NewYork extends Market("New York")
    case object Tokyo extends Market("Tokyo")
    case object Singapore extends Market("Singapore")
    case object HongKong extends Market("Hong Kong")
    case object Other extends Market("Other")

    val values = findValues
  }
}
