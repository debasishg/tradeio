package tradex.domain
package trading

import java.util.UUID
import cats.data.{NonEmptyList, EitherNec}
import cats.implicits._
import cats.instances.list._

import NewtypeRefinedOps._
import model.order._
import model.newtypes._
import model.execution._

object AppData {
  val ano1String = "ibm-123"
  val ano2String = "ibm-124"
  val ano1 = validate[AccountNo]("ibm-123")
    .fold(errs => throw new Exception(errs.toString), identity)
  val ano2 = validate[AccountNo]("ibm-124")
    .fold(errs => throw new Exception(errs.toString), identity)
  val ano3 = validate[AccountNo]("nri-654")
    .fold(errs => throw new Exception(errs.toString), identity)

  val appleISIN = "US0378331005"
  val baeISIN = "GB0002634946"
  val ibmISIN = "US4592001014"

  val lis = NonEmptyList
    .of(
      Order.makeLineItem(appleISIN, 100, 12.25, "buy"),
      Order.makeLineItem(baeISIN, 200, 52.25, "sell"),
      Order.makeLineItem(appleISIN, 100, 32.25, "buy")
    )
    .toList
    .sequence

  val order: EitherNec[String, Order] = lis
    .map { lineItems =>
      Order.makeOrder(
        "o1",
        today,
        ano1String,
        NonEmptyList.fromList(lineItems).get
      )
    }
    .fold(Left(_), identity)

  val o1 = order.fold(errs => throw new Exception(errs.toString), identity)

  val e1 = Execution
    .execution(
      executionRefNo = UUID.randomUUID().toString(),
      accountNo = ano1String,
      orderNo = "o1",
      isin = appleISIN,
      market = "New York",
      buySell = "buy",
      unitPrice = 12.25,
      quantity = 100,
      dateOfExecution = today
    )
    .fold(errs => throw new Exception(errs.toString), identity)

  val e2 = Execution
    .execution(
      executionRefNo = UUID.randomUUID().toString(),
      accountNo = ano1String,
      orderNo = "o1",
      isin = baeISIN,
      market = "New York",
      buySell = "sell",
      unitPrice = 52.25,
      quantity = 100,
      dateOfExecution = today
    )
    .fold(errs => throw new Exception(errs.toString), identity)

  val e3 = Execution
    .execution(
      executionRefNo = UUID.randomUUID().toString(),
      accountNo = ano1String,
      orderNo = "o1",
      isin = baeISIN,
      market = "New York",
      buySell = "buy",
      unitPrice = 51.25,
      quantity = 100,
      dateOfExecution = today
    )
    .fold(errs => throw new Exception(errs.toString), identity)

  val e4 = Execution
    .execution(
      executionRefNo = UUID.randomUUID().toString(),
      accountNo = ano1String,
      orderNo = "o1",
      isin = ibmISIN,
      market = "New York",
      buySell = "buy",
      unitPrice = 32.25,
      quantity = 100,
      dateOfExecution = today
    )
    .fold(errs => throw new Exception(errs.toString), identity)
}
