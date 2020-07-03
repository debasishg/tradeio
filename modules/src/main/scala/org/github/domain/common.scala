package org.github.domain

import java.time.LocalDateTime

import cats._
import cats.data._
import io.estatico.newtype.macros.newtype

object common {
  type ValidationResult[A] = ValidatedNec[String, A]
  type ErrorOr[A] = Either[NonEmptyChain[String], A]
  type MonadThrowable[F[_]] = MonadError[F, Throwable]

  @newtype case class Quantity(value: BigDecimal)

  def today = LocalDateTime.now
}
