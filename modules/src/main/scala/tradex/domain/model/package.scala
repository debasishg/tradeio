package tradex.domain

import java.time.LocalDateTime
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import cats.{Eq, Monoid, Show}
import cats.syntax.all._
import io.circe.{Decoder, Encoder}
import squants.market.{Currency, Money, USD}

package object model extends OrphanInstances

// instances for types we don't control
trait OrphanInstances {
  implicit val showLocalDateTime: Show[LocalDateTime] =
    Show[String].contramap[LocalDateTime] { p =>
      p.format(DateTimeFormatter.ISO_DATE).show
    }

  implicit val moneyDecoder: Decoder[Money] =
    Decoder[BigDecimal].map(USD.apply)

  implicit val currencyDecoder: Decoder[Currency] =
    Decoder[String].map(Currency.apply(_).get)

  implicit val moneyEncoder: Encoder[Money] =
    Encoder[BigDecimal].contramap(_.amount)

  implicit val currencyEncoder: Encoder[Currency] =
    Encoder[String].contramap(_.toString)

  implicit val moneyMonoid: Monoid[Money] =
    new Monoid[Money] {
      def empty: Money = USD(0)
      def combine(x: Money, y: Money): Money = x + y
    }

  implicit val dateTimeEq: Eq[LocalDateTime] =
    Eq.by(_.atZone(ZoneId.systemDefault).toEpochSecond)

  implicit val currencyEq: Eq[Currency] =
    Eq.and(Eq.and(Eq.by(_.code), Eq.by(_.symbol)), Eq.by(_.name))

  implicit val moneyEq: Eq[Money] = Eq.and(Eq.by(_.amount), Eq.by(_.currency))

  implicit val showCurrency: Show[Currency] =
    Show[String].contramap[Currency] { c =>
      s"Name: ${c.name.show}, Code: ${c.code.show}, Symbol: ${c.symbol.show}"
    }

  implicit val showMoney: Show[Money] =
    Show[String].contramap[Money](_.toString)
}
