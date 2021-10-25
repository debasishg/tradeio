package tradex.domain
package programs

import cats.data._
import cats.syntax.all._

import io.chrisdavenport.cormorant._
import io.chrisdavenport.cormorant.generic.semiauto._
import io.chrisdavenport.cormorant.implicits._

import model.order._
import model.trade._
import model.balance._
import model.user.UserId

import services.trading._
import services.accounting._

final case class GenerateTrade[F[_]: MonadThrowable] private (
    trading: Trading[F],
    accounting: Accounting[F]
) {
  val F = implicitly[MonadThrowable[F]]
  def generate(
      frontOfficeInput: GenerateTradeFrontOfficeInput,
      userId: UserId
  ): F[(NonEmptyList[Trade], NonEmptyList[Balance])] = F.uncancelable { _ =>
    // making this operation uncancelable
    // Is this the proper thinking ?
    // Note database transactions are involved - hence cancelations can be tricky
    import trading._
    import accounting._

    val action = for {
      orders <- orders(frontOfficeInput.frontOfficeOrders)
      executions <- execute(
        orders,
        frontOfficeInput.market,
        frontOfficeInput.brokerAccountNo
      )
      trades   <- allocate(executions, frontOfficeInput.clientAccountNos, userId)
      balances <- postBalance(trades)
    } yield (trades, balances)

    action.adaptError {
      case oe: Trading.TradingError => Trading.TradeGenerationError(oe.cause)
      case e                        => Trading.TradeGenerationError(e.getMessage())
    }
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
