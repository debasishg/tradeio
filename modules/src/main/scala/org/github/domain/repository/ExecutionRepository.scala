package org.github.domain
package repository

import cats.data.NonEmptyList
import model.execution._

trait ExecutionRepository[M[_]] {
  /** store */
  def store(exe: Execution): M[Execution]
  /** store many executions */
  def store(executions: NonEmptyList[Execution]): M[Unit]
}
