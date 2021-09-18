package tradex.domain
package model

import java.util.UUID

import cats.syntax.all._
import cats.data.ValidatedNec
import cats.Functor

import account._
import instrument._
import order._
import market._
import java.time.LocalDateTime
import io.estatico.newtype.macros.newtype
import derevo.cats._
import derevo.circe.magnolia._
import derevo.derive
import optics.uuid
import effects.GenUUID

object execution {
  @derive(decoder, encoder, eqv, show, uuid)
  @newtype
  @newtype case class ExecutionReferenceNo(value: UUID)

  // primary domain entity for execution from exchange
  @derive(decoder, encoder, eqv, show)
  final case class Execution private (
      executionRefNo: ExecutionReferenceNo,
      accountNo: AccountNo,
      orderNo: OrderNo,
      isin: ISINCode,
      market: Market,
      buySell: BuySell,
      unitPrice: UnitPrice,
      quantity: Quantity,
      dateOfExecution: LocalDateTime,
      exchangeExecutionRefNo: Option[String] = None
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
    // smart constructor : adds validation
    def execution[F[_]: Functor: GenUUID](
        accountNo: AccountNo,
        orderNo: OrderNo,
        isin: ISINCode,
        market: Market,
        buySell: BuySell,
        unitPrice: UnitPrice,
        quantity: Quantity,
        dateOfExecution: LocalDateTime
    ): F[Execution] = {
      ID.make[F, ExecutionReferenceNo]
        .map { refNo =>
          Execution(
            refNo,
            accountNo,
            orderNo,
            isin,
            market,
            buySell,
            unitPrice,
            quantity,
            dateOfExecution,
            None
          )
        }
    }

    def execution[F[_]: Functor: GenUUID](
        accountNo: String,
        orderNo: String,
        isin: String,
        market: String,
        buySell: String,
        unitPrice: BigDecimal,
        quantity: BigDecimal,
        dateOfExecution: LocalDateTime
    ): ValidatedNec[String, F[Execution]] = {
      (
        Account.validateAccountNo(accountNo),
        Order.validateOrderNo(orderNo),
        Instrument.validateISINCode(isin),
        validateMarket(market),
        Order.validateBuySell(buySell),
        Order.validateUnitPrice(unitPrice),
        Order.validateQuantity(quantity)
      ).mapN { (ano, ono, isin, m, bs, up, qty) =>
        ID.make[F, ExecutionReferenceNo]
          .map { id =>
            Execution(
              id,
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
    }

    // smart constructor from data received from exchange
    private[domain] def createExecution[F[_]: Functor: GenUUID](
        eex: ExchangeExecution
    ): ValidatedNec[String, F[Execution]] = {
      (
        Account.validateAccountNo(eex.accountNo),
        Instrument.validateISINCode(eex.isin),
        Order.validateBuySell(eex.buySell),
        Order.validateUnitPrice(eex.unitPrice),
        Order.validateQuantity(eex.quantity),
        Order.validateOrderNo(eex.orderNo)
      ).mapN { (ano, ins, bs, up, q, ono) =>
        ID.make[F, ExecutionReferenceNo]
          .map { id =>
            Execution(
              id,
              ano,
              ono,
              ins,
              Market.withName(eex.market),
              BuySell.withName(bs),
              up,
              q,
              eex.dateOfExecution,
              if (eex.executionRefNo.isEmpty()) None
              else Some(eex.executionRefNo)
            )
          }
      }
    }

    private[model] def validateMarket(m: String): ValidatedNec[String, Market] = {
      Market
        .withNameEither(m)
        .toValidatedNec
        .leftMap(_.map(_.toString))
    }
  }
}
