package tradex.domain

import cats.data.{ NonEmptyList, ValidatedNec }
import cats.data.Validated._
import cats.syntax.all._
import cats.instances.list._
import cats.effect.IO
import cats.effect.unsafe.implicits.global

import NewtypeRefinedOps._
import model.account._
import model.order._
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
  val baeISIN   = "GB0002634946"
  val ibmISIN   = "US4592001014"

  val lis = NonEmptyList
    .of(
      Order.makeLineItem("o1", appleISIN, 100, 12.25, "buy"),
      Order.makeLineItem("o1", baeISIN, 200, 52.25, "sell"),
      Order.makeLineItem("o1", appleISIN, 100, 32.25, "buy")
    )
    .toList
    .sequence

  val order: ValidatedNec[String, Order] = lis match {
    case Invalid(e) => e.toList.mkString("/").invalidNec
    case Valid(lineItems) =>
      Order.makeOrder(
        "o1",
        today,
        ano1String,
        NonEmptyList.fromList(lineItems).get
      )
  }

  val o1 = order.fold(errs => throw new Exception(errs.toString), identity)

  val e1: IO[Execution] = Execution
    .execution[IO](
      accountNo = ano1String,
      orderNo = "o1",
      isin = appleISIN,
      market = "Tokyo",
      buySell = "buy",
      unitPrice = 12.25,
      quantity = 100,
      dateOfExecution = today
    )
    .fold(errs => throw new Exception(errs.toString), identity)

  val e2: IO[Execution] = Execution
    .execution[IO](
      accountNo = ano1String,
      orderNo = "o1",
      isin = baeISIN,
      market = "Tokyo",
      buySell = "sell",
      unitPrice = 52.25,
      quantity = 100,
      dateOfExecution = today
    )
    .fold(errs => throw new Exception(errs.toString), identity)

  val e3: IO[Execution] = Execution
    .execution[IO](
      accountNo = ano1String,
      orderNo = "o1",
      isin = baeISIN,
      market = "Tokyo",
      buySell = "buy",
      unitPrice = 51.25,
      quantity = 100,
      dateOfExecution = today
    )
    .fold(errs => throw new Exception(errs.toString), identity)

  val e4: IO[Execution] = Execution
    .execution[IO](
      accountNo = ano1String,
      orderNo = "o1",
      isin = appleISIN,
      market = "Tokyo",
      buySell = "buy",
      unitPrice = 32.25,
      quantity = 100,
      dateOfExecution = today
    )
    .fold(errs => throw new Exception(errs.toString), identity)

  val executions: List[Execution] = List(e1, e2, e3, e4).sequence.unsafeRunSync()
}
