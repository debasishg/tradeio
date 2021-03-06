package tradex.domain
package trading

import cats.effect._
import cats.implicits._

import skunk._

import repository._

object Algebras {
  // smart constructor for skunk based algebras
  def make[F[_]: Sync](
      sessionPool: Resource[F, Session[F]]
  ): F[Algebras[F]] = {
    import repository.interpreter.skunk._
    for {
      accountRepositoryInterpreter <- AccountRepositoryInterpreter.make[F](
        sessionPool
      )
      executionRepositoryInterpreter <- ExecutionRepositoryInterpreter.make[F](
        sessionPool
      )
      instrumentRepositoryInterpreter <- InstrumentRepositoryInterpreter
        .make[F](sessionPool)
      orderRepositoryInterpreter <- OrderRepositoryInterpreter.make[F](
        sessionPool
      )
      tradeRepositoryInterpreter <- TradeRepositoryInterpreter.make[F](
        sessionPool
      )
      balanceRepositoryInterpreter <- BalanceRepositoryInterpreter.make[F](
        sessionPool
      )
    } yield new Algebras[F](
      accountRepositoryInterpreter,
      executionRepositoryInterpreter,
      instrumentRepositoryInterpreter,
      orderRepositoryInterpreter,
      tradeRepositoryInterpreter,
      balanceRepositoryInterpreter
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
