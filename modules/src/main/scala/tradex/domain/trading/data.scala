package tradex.domain
package trading

import java.util.UUID
import cats.data.NonEmptyList
import cats.implicits._
import cats.instances.list._

import common._
import model.order._
import model.execution._

object AppData {
  val ano1 = "ibm-123"
  val ano2 = "ibm-124"
  val ano3 = "nri-654"

  val appleISIN = "US0378331005"
  val baeISIN = "GB0002634946"
  val ibmISIN = "US4592001014"

  val lis = NonEmptyList.of(
    Order.makeLineItem(appleISIN, 100, 12.25, "buy"),
    Order.makeLineItem(baeISIN, 200, 52.25, "sell"),
    Order.makeLineItem(appleISIN, 100, 32.25, "buy")
  ).toList.sequence

  val order: ValidationResult[Order] = lis.map { lineItems =>
    Order.makeOrder("o1", today, ano1, NonEmptyList.fromList(lineItems).get)
  }.fold(_.invalid[Order], identity)

  val e1 = Execution.execution(
    executionRefNo = UUID.randomUUID().toString(),
    accountNo = ano1,
    orderNo = "o1",
    isin = appleISIN,
    market = "New York",
    buySell = "buy",
    unitPrice = 12.25,
    quantity = 100,
    dateOfExecution = today
  )

  val e2 = Execution.execution(
    executionRefNo = UUID.randomUUID().toString(),
    accountNo = ano1,
    orderNo = "o1",
    isin = baeISIN,
    market = "New York",
    buySell = "sell",
    unitPrice = 52.25,
    quantity = 100,
    dateOfExecution = today
  )

  val e3 = Execution.execution(
    executionRefNo = UUID.randomUUID().toString(),
    accountNo = ano1,
    orderNo = "o1",
    isin = baeISIN,
    market = "New York",
    buySell = "buy",
    unitPrice = 51.25,
    quantity = 100,
    dateOfExecution = today
  )

  val e4 = Execution.execution(
    executionRefNo = UUID.randomUUID().toString(),
    accountNo = ano1,
    orderNo = "o1",
    isin = ibmISIN,
    market = "New York",
    buySell = "buy",
    unitPrice = 32.25,
    quantity = 100,
    dateOfExecution = today
  )
}
