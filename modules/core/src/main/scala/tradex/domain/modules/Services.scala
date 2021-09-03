package tradex.domain
package modules

import cats.effect._
import dev.profunktor.redis4cats.RedisCommands
import skunk.Session
import repository._
import services.trading._
import services.accounting._
import services.healthcheck._

object Services {
  def make[F[+_]: Temporal](
      postgres: Resource[F, Session[F]],
      redis: RedisCommands[F, String, String]
  ): Services[F] = {
    val _accountRepository    = AccountRepository.make(postgres)
    val _instrumentRepository = InstrumentRepository.make(postgres)
    val _orderRepository      = OrderRepository.make(postgres)
    val _executionRepository  = ExecutionRepository.make(postgres)
    val _tradeRepository      = TradeRepository.make(postgres)
    val _balanceRepository    = BalanceRepository.make(postgres)

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
      Accounting.make(_balanceRepository),
      HealthCheck.make(postgres, redis)
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
    val accounting: Accounting[F],
    val healthCheck: HealthCheck[F]
)
