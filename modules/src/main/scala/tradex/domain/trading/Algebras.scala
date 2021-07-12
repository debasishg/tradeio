package tradex.domain
package trading

import cats.effect._
import cats.implicits._

import skunk._

import repository._

object Algebras {
  // smart constructor for skunk based algebras
  def make[F[_]: Concurrent](
      postgres: Resource[F, Session[F]]
  ): Algebras[F] = {
    import repository.interpreter.skunk._
    new Algebras[F](
      AccountRepositoryInterpreter.make[F](postgres),
      ExecutionRepositoryInterpreter.make[F](postgres),
      InstrumentRepositoryInterpreter.make[F](postgres),
      OrderRepositoryInterpreter.make[F](postgres),
      TradeRepositoryInterpreter.make[F](postgres),
      BalanceRepositoryInterpreter.make[F](postgres)
    )
  }

  // smart constructor for memory based algebras
  def make[F[_]: Sync]: F[Algebras[F]] = {
    import repository.interpreter.memory._
    for {
      accountRepositoryInterpreter <- AccountRepositoryInterpreter.make[F]
      executionRepositoryInterpreter <- ExecutionRepositoryInterpreter.make[F]
      instrumentRepositoryInterpreter <- InstrumentRepositoryInterpreter.make[F]
      orderRepositoryInterpreter <- OrderRepositoryInterpreter.make[F]
      tradeRepositoryInterpreter <- TradeRepositoryInterpreter.make[F]
      balanceRepositoryInterpreter <- BalanceRepositoryInterpreter.make[F]
    } yield new Algebras[F](
      accountRepositoryInterpreter,
      executionRepositoryInterpreter,
      instrumentRepositoryInterpreter,
      orderRepositoryInterpreter,
      tradeRepositoryInterpreter,
      balanceRepositoryInterpreter
    )
  }
}

final class Algebras[F[_]] private (
    val accountRepository: AccountRepository[F],
    val executionRepository: ExecutionRepository[F],
    val instrumentRepository: InstrumentRepository[F],
    val orderRepository: OrderRepository[F],
    val tradeRepository: TradeRepository[F],
    val balanceRepository: BalanceRepository[F]
)
