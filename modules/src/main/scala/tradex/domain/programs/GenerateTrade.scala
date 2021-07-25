package tradex.domain
package programs

import cats.data._
import cats.syntax.all._

import org.typelevel.log4cats.Logger

import io.chrisdavenport.cormorant._
import io.chrisdavenport.cormorant.generic.semiauto._
import io.chrisdavenport.cormorant.implicits._

import model.order._
import model.trade._
import model.balance._

import services.trading._
import services.accounting._

final case class GenerateTrade[F[_]: Logger: MonadThrowable] private (
    trading: Trading[F],
    accounting: Accounting[F]
) {
  def generate(
      frontOfficeInput: GenerateTradeFrontOfficeInput
  ): F[(NonEmptyList[Trade], NonEmptyList[Balance])] = {
    import trading._
    import accounting._

    for {
      orders <- orders(frontOfficeInput.frontOfficeOrders)
      executions <- execute(
        orders,
        frontOfficeInput.market,
        frontOfficeInput.brokerAccountNo
      )
      trades <- allocate(executions, frontOfficeInput.clientAccountNos)
      balances <- postBalance(trades)
    } yield (trades, balances)
  }
}

// generate order from front office
private[programs] object orderGenerator {
  def generateOrders(
      foOrders: NonEmptyList[FrontOfficeOrder]
  ): String = {
    implicit val lw: LabelledWrite[FrontOfficeOrder] = deriveLabelledWrite
    foOrders.toList.writeComplete.print(Printer.default)
  }
}
