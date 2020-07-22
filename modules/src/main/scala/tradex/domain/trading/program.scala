package tradex.domain
package trading

import java.time.Instant

import cats._
import cats.data._
import cats.implicits._

import io.chrisdavenport.cormorant._
import io.chrisdavenport.cormorant.generic.semiauto._
import io.chrisdavenport.cormorant.implicits._

import model.newtypes._
import model.market._
import model.order._
import model.trade._

import AppData._

object program {
  def tradeGeneration[F[_]: FlatMap](
      trading: Trading[F],
      csvOrder: String,
      brokerAccountNo: AccountNo,
      market: Market,
      clientAccountNos: NonEmptyList[AccountNo]
  ): F[NonEmptyList[Trade]] = {
    import trading._
    for {
      orders <- orders(csvOrder)
      executions <- execute(orders, market, brokerAccountNo)
      trades <- allocate(executions, clientAccountNos)
    } yield trades
  }
}

// generate order from front office
object orderGenerator {
  def generateOrders(): String = {
    val o1 =
      FrontOfficeOrder(
        ano1.value,
        Instant.now(),
        "US0378331005",
        100.00,
        1200.50,
        "buy"
      )
    val o2 =
      FrontOfficeOrder(
        ano1.value,
        Instant.now(),
        "GB0002634946",
        200.00,
        230.00,
        "sell"
      )
    val o3 =
      FrontOfficeOrder(
        ano2.value,
        Instant.now(),
        "US0378331005",
        100.00,
        1200.50,
        "buy"
      )

    val orders = List(o1, o2, o3)
    implicit val lw: LabelledWrite[FrontOfficeOrder] = deriveLabelledWrite
    orders.writeComplete.print(Printer.default)
  }
}
