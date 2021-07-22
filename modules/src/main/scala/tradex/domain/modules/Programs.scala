package tradex.domain
package modules

import cats.effect._
import org.typelevel.log4cats.Logger
import programs.GenerateTrade

object Programs {
  def make[F[+_]: Logger: Temporal](
      services: Services[F]
  ): Programs[F] =
    new Programs[F](services) {}
}

sealed abstract class Programs[F[+_]: Temporal: Logger] private (
    services: Services[F]
) {
  val generateTrade =
    GenerateTrade(services.trading, services.accounting)
}
