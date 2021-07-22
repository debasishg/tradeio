package tradex.domain
package modules

import cats.effect._
import org.typelevel.log4cats.Logger
import programs.GenerateTrade

sealed abstract class Programs[F[+_]: Temporal: Logger] private (
    services: Services[F]
) {
  val generateTrade =
    GenerateTrade(services.trading, services.accounting)
}
