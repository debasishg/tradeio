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
  val ano1 = AccountNo("ibm-123")
  val ano2 = AccountNo("ibm-124")
  val ano3 = AccountNo("nri-654")

  val apple = ISINCode("US0378331005")
  val bae = ISINCode("GB0002634946")
  val ibm = ISINCode("US4592001014")

  val o1 = Order(
    no = OrderNo("o1"),
    date = today,
    accountNo = ano1,
    items = NonEmptyList
      .fromList(
        List(
          LineItem(apple, Quantity(100), UnitPrice(12.25), BuySell.Buy),
          LineItem(bae, Quantity(200), UnitPrice(52.25), BuySell.Sell),
          LineItem(apple, Quantity(100), UnitPrice(32.25), BuySell.Buy)
        )
      )
      .get
  )

  val e1 = Execution(
    executionRefNo = ExecutionReferenceNo(UUID.randomUUID().toString()),
    accountNo = ano1,
    orderNo = o1.no,
    isin = apple,
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
    isin = bae,
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
    isin = bae,
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
    isin = ibm,
    market = Market.NewYork,
    buySell = BuySell.Buy,
    unitPrice = UnitPrice(32.25),
    quantity = Quantity(100),
    dateOfExecution = today
  )
}
