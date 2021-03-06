package tradex.domain
package model

import io.estatico.newtype.macros.newtype

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric._
import eu.timepit.refined.collection._
import eu.timepit.refined.boolean.AllOf
import eu.timepit.refined.types.string.NonEmptyString

import _root_.shapeless.::
import _root_.shapeless.HNil
import eu.timepit.refined.string.MatchesRegex

object newtypes {
  // account
  type AccountNoString = String Refined AllOf[
    MaxSize[W.`12`.T] ::
      MinSize[W.`5`.T] ::
      HNil
  ]

  @newtype case class AccountNo(value: AccountNoString)
  @newtype case class AccountName(value: NonEmptyString)

  // instrument
  type ISINCodeString = String Refined AllOf[
    MaxSize[W.`12`.T] ::
      MinSize[W.`12`.T] ::
      MatchesRegex[W.`"([A-Z]{2})((?![A-Z]{10}\b)[A-Z0-9]{10})"`.T] ::
      HNil
  ]

  @newtype case class ISINCode(value: ISINCodeString)
  @newtype case class InstrumentName(value: NonEmptyString)
  @newtype case class LotSize(value: Short Refined Positive)

  // order
  @newtype case class OrderNo(value: NonEmptyString)
  @newtype case class Quantity(value: BigDecimal Refined NonNegative)

  // execution
  @newtype case class ExecutionReferenceNo(value: NonEmptyString)
  @newtype case class UnitPrice(value: BigDecimal Refined Positive)

  // trade
  @newtype case class TradeReferenceNo(value: NonEmptyString)
}
