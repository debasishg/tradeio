package tradex.domain
package modules

import cats.effect._
import org.typelevel.log4cats.Logger
import skunk.Session
import repository._
import trading._
import accounting._

object Services {
  def make[F[+_]: Concurrent: Logger](
      postgres: Resource[F, Session[F]]
  ): Services[F] = {
    val _accountRepository = AccountRepository.make(postgres)
    val _instrumentRepository = InstrumentRepository.make(postgres)
    val _orderRepository = OrderRepository.make(postgres)
    val _executionRepository = ExecutionRepository.make(postgres)
    val _tradeRepository = TradeRepository.make(postgres)
    val _balanceRepository = BalanceRepository.make(postgres)

    new Services[F](
      _accountRepository,
      _instrumentRepository,
      _orderRepository,
      _executionRepository,
      _tradeRepository,
      _balanceRepository,
      Trading.make(
        _accountRepository,
        _executionRepository,
        _orderRepository,
        _tradeRepository
      ),
      Accounting.make(_balanceRepository)
    ) {}
  }
}

sealed abstract class Services[F[+_]] private (
    val accountRepository: AccountRepository[F],
    val instrumentRepository: InstrumentRepository[F],
    val orderRepository: OrderRepository[F],
    val executionRepository: ExecutionRepository[F],
    val tradeRepository: TradeRepository[F],
    val balanceRepository: BalanceRepository[F],
    val trading: Trading[F],
    val accounting: Accounting[F]
)
