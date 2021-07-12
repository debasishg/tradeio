package tradex.domain
package trading

import java.time.Instant

import cats.data._
import cats.implicits._
import cats.effect.Sync

import org.typelevel.log4cats.Logger

import io.chrisdavenport.cormorant._
import io.chrisdavenport.cormorant.generic.semiauto._
import io.chrisdavenport.cormorant.implicits._

import model.market._
import model.order._
import model.trade._
import model.balance._

import accounting._

import AppData._

object Programs {
  def make[F[_]: Logger: MonadThrowable: Sync](
      algebras: Algebras[F]
  ): F[Programs[F]] = {
    Sync[F].delay(new Programs[F](algebras))
  }
}

final class Programs[F[_]: Logger: MonadThrowable] private (
    algebras: Algebras[F]
) {
  def generateTrade(
      trading: Trading[F],
      accounting: Accounting[F]
  ): F[(NonEmptyList[Trade], NonEmptyList[Balance])] = {
    import trading._
    import accounting._

    val csvOrder = orderGenerator.generateOrders()
    val brokerAccountNo = ano3
    val clientAccountNos = NonEmptyList.of(ano1, ano2)

    for {
      orders <- orders(csvOrder)
      executions <- execute(orders, Market.NewYork, brokerAccountNo)
      trades <- allocate(executions, clientAccountNos)
      balances <- postBalance(trades)
    } yield (trades, balances)
  }
}

// generate order from front office
private[trading] object orderGenerator {
  def generateOrders(): String = {
    val o1 =
      FrontOfficeOrder(
        ano1String,
        Instant.now(),
        "US0378331005",
        100.00,
        1200.50,
        "buy"
      )
    val o2 =
      FrontOfficeOrder(
        ano1String,
        Instant.now(),
        "GB0002634946",
        200.00,
        230.00,
        "sell"
      )
    val o3 =
      FrontOfficeOrder(
        ano2String,
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
