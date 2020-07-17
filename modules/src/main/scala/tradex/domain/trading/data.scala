package tradex.domain
package trading

import java.util.UUID
import cats.data.NonEmptyList

import common._
import model.instrument._
import model.account._
import model.order._
import model.execution._
import model.newtypes._
import model.enums._
import model.market._

object AppData {
  val ano1 = AccountNo("a-no1")
  val ano2 = AccountNo("a-no2")
  val ano3 = AccountNo("a-no3")

  val isin1 = ISINCode("012345678901")
  val isin2 = ISINCode("123456789010")
  val isin3 = ISINCode("234567890101")

  val o1 = Order(
    no = OrderNo("o1"),
    date = today,
    accountNo = ano1,
    items = NonEmptyList
      .fromList(
        List(
          LineItem(isin1, Quantity(100), UnitPrice(12.25), BuySell.Buy),
          LineItem(isin2, Quantity(200), UnitPrice(52.25), BuySell.Sell),
          LineItem(isin3, Quantity(100), UnitPrice(32.25), BuySell.Buy)
        )
      )
      .get
  )

  val e1 = Execution(
    executionRefNo = ExecutionReferenceNo(UUID.randomUUID().toString()),
    accountNo = ano1,
    orderNo = o1.no,
    isin = isin1,
    market = Market.NewYork,
    buySell = BuySell.Buy,
    unitPrice = UnitPrice(12.25),
    quantity = Quantity(100),
    dateOfExecution = today
  )

  val e2 = Execution(
    executionRefNo = ExecutionReferenceNo(UUID.randomUUID().toString()),
    accountNo = ano1,
    orderNo = o1.no,
    isin = isin2,
    market = Market.NewYork,
    buySell = BuySell.Sell,
    unitPrice = UnitPrice(52.25),
    quantity = Quantity(100),
    dateOfExecution = today
  )

  val e3 = Execution(
    executionRefNo = ExecutionReferenceNo(UUID.randomUUID().toString()),
    accountNo = ano1,
    orderNo = o1.no,
    isin = isin2,
    market = Market.NewYork,
    buySell = BuySell.Sell,
    unitPrice = UnitPrice(51.25),
    quantity = Quantity(100),
    dateOfExecution = today
  )

  val e4 = Execution(
    executionRefNo = ExecutionReferenceNo(UUID.randomUUID().toString()),
    accountNo = ano1,
    orderNo = o1.no,
    isin = isin3,
    market = Market.NewYork,
    buySell = BuySell.Buy,
    unitPrice = UnitPrice(32.25),
    quantity = Quantity(100),
    dateOfExecution = today
  )
}
