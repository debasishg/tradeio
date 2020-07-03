package org.github.domain
package model

import cats.implicits._

import enumeratum._
import enumeratum.EnumEntry._

import common._
import newtypes._
import account._
import instrument._
import market._

object execution {

  private[model] final case class Execution(
    accountNo: AccountNo, 
    isin: ISINCode, 
    refNo: ReferenceNo, 
    market: Market,
    unitPrice: UnitPrice, 
    quantity: Quantity
  )

  object Execution {

    def execution(exe: Execution): ErrorOr[Execution] = {
      (

        Account.validateAccountNo(exe.accountNo),
        Instrument.validateISINCode(exe.isin),
        validateQuantity(exe.quantity),
        validateUnitPrice(exe.unitPrice)

      ).mapN { (ano, ins, q, p) =>
        Execution(ano, ins, exe.refNo, exe.market, p, q)
      }.toEither
    }

    private[model] def validateQuantity(qty: Quantity): ValidationResult[Quantity] =
      if (qty.value <= 0)
        s"Quantity has to be positive: found $qty".invalidNec
      else qty.validNec

    private[model] def validateUnitPrice(price: UnitPrice): ValidationResult[UnitPrice] =
      if (price.value <= 0)
        s"Unit Price has to be positive: found $price".invalidNec
      else price.validNec
  }
}
