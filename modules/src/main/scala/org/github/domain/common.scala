package org.github.domain

import java.time.LocalDateTime

import cats._
import cats.data._

object common {
  type ValidationResult[A] = ValidatedNec[String, A]
  type ErrorOr[A] = Either[NonEmptyChain[String], A]
  type MonadThrowable[F[_]] = MonadError[F, Throwable]

  def today = LocalDateTime.now
}
