package org.github.domain
package model

import cats.implicits._

import common._
import newtypes._
import account._
import instrument._
import market._
import enums._
import java.time.LocalDateTime

object execution {
  // primary domain entity
  private[domain] final case class Execution(
      executionRefNo: ExecutionReferenceNo,
      accountNo: AccountNo,
      orderNo: OrderNo,
      isin: ISINCode,
      market: Market,
      buySell: BuySell,
      unitPrice: UnitPrice,
      quantity: Quantity,
      dateOfExecution: LocalDateTime
  )

  // as per layout obtained from exchange
  private[domain] final case class ExchangeExecution(
      executionRefNo: String,
      accountNo: String,
      orderNo: String,
      isin: String,
      market: String,
      buySell: String,
      unitPrice: BigDecimal,
      quantity: BigDecimal,
      dateOfExecution: LocalDateTime
  )

  object Execution {
    // smart constructor
    def execution(exe: Execution): ErrorOr[Execution] = {
      (
        Account.validateAccountNo(exe.accountNo),
        Instrument.validateISINCode(exe.isin),
        validateBuySell(exe.buySell.entryName),
        validateQuantity(exe.quantity),
        validateUnitPrice(exe.unitPrice)
      ).mapN { (ano, ins, bs, q, p) =>
        Execution(
          exe.executionRefNo,
          ano,
          exe.orderNo,
          ins,
          exe.market,
          BuySell.withName(bs),
          p,
          q,
          exe.dateOfExecution
        )
      }.toEither
    }

    // smart constructor from data received from exchange
    private[domain] def createExecution(
        eex: ExchangeExecution
    ): ErrorOr[Execution] = {
      (
        Account.validateAccountNo(AccountNo(eex.accountNo)),
        Instrument.validateISINCode(ISINCode(eex.isin)),
        validateBuySell(eex.buySell),
        validateUnitPrice(UnitPrice(eex.unitPrice)),
        validateQuantity(Quantity(eex.quantity))
      ).mapN { (ano, ins, bs, up, q) =>
        Execution(
          ExecutionReferenceNo(eex.executionRefNo),
          ano,
          OrderNo(eex.orderNo),
          ins,
          Market.withName(eex.market),
          BuySell.withName(bs),
          up,
          q,
          eex.dateOfExecution
        )
      }.toEither
    }

    private[model] def validateQuantity(
        qty: Quantity
    ): ValidationResult[Quantity] =
      if (qty.value <= 0)
        s"Quantity has to be positive: found $qty".invalidNec
      else qty.validNec

    private[model] def validateUnitPrice(
        price: UnitPrice
    ): ValidationResult[UnitPrice] =
      if (price.value <= 0)
        s"Unit Price has to be positive: found $price".invalidNec
      else price.validNec

    private[model] def validateBuySell(bs: String): ValidationResult[String] = {
      BuySell
        .withNameEither(bs)
        .toValidatedNec
        .map(_.entryName)
        .leftMap(_.map(_.toString))
    }
  }
}
