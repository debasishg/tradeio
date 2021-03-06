package tradex.domain
package model

import java.util.UUID

import cats.implicits._
import cats.data.EitherNec

import NewtypeRefinedOps._
import newtypes._
import account._
import instrument._
import order._
import market._
import enums._
import java.time.LocalDateTime

object execution {
  // primary domain entity
  private[domain] final case class Execution private (
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
  private[domain] final case class ExchangeExecution private (
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
    def execution(
        executionRefNo: String,
        accountNo: String,
        orderNo: String,
        isin: String,
        market: String,
        buySell: String,
        unitPrice: BigDecimal,
        quantity: BigDecimal,
        dateOfExecution: LocalDateTime
    ): EitherNec[String, Execution] = {
      (
        validateExecutionRefNo(executionRefNo),
        Account.validateAccountNo(accountNo),
        Order.validateOrderNo(orderNo),
        Instrument.validateISINCode(isin),
        validateMarket(market),
        Order.validateBuySell(buySell),
        Order.validateUnitPrice(unitPrice),
        Order.validateQuantity(quantity)
      ).parMapN { (ref, ano, ono, isin, m, bs, up, qty) =>
        Execution(
          ref,
          ano,
          ono,
          isin,
          m,
          BuySell.withName(bs),
          up,
          qty,
          dateOfExecution
        )
      }
    }

    // smart constructor from data received from exchange
    private[domain] def createExecution(
        eex: ExchangeExecution
    ): EitherNec[String, Execution] = {
      (
        validateExecutionRefNo(eex.executionRefNo),
        Account.validateAccountNo(eex.accountNo),
        Instrument.validateISINCode(eex.isin),
        Order.validateBuySell(eex.buySell),
        Order.validateUnitPrice(eex.unitPrice),
        Order.validateQuantity(eex.quantity),
        Order.validateOrderNo(eex.orderNo)
      ).parMapN { (ref, ano, ins, bs, up, q, ono) =>
        Execution(
          ref,
          ano,
          ono,
          ins,
          Market.withName(eex.market),
          BuySell.withName(bs),
          up,
          q,
          eex.dateOfExecution
        )
      }
    }

    private[model] def validateExecutionRefNo(
        refNo: String
    ): EitherNec[String, ExecutionReferenceNo] = {
      validate[ExecutionReferenceNo](refNo)
    }

    private[model] def validateMarket(m: String): EitherNec[String, Market] = {
      Market
        .withNameEither(m)
        .toEitherNec
        .leftMap(_.map(_.toString))
    }

    def generateExecutionReferenceNo(): ExecutionReferenceNo =
      validateExecutionRefNo(UUID.randomUUID().toString)
        .fold(
          errs =>
            throw new Exception(
              s"Unable to generate reference no : ${errs.toString}"
            ),
          identity
        )
  }
}
