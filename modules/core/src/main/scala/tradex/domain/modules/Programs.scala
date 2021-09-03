package tradex.domain
package modules

import cats.effect._
import programs.GenerateTrade

object Programs {
  def make[F[+_]: Temporal](
      services: Services[F]
  ): Programs[F] =
    new Programs[F](services) {}
}

sealed abstract class Programs[F[+_]: Temporal] private (
    services: Services[F]
) {
  val generateTrade =
    GenerateTrade(services.trading, services.accounting)
}
