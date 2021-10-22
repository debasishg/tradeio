package tradex.domain
package services.trading

import java.io.InputStream

import cats.Functor
import cats.data.{ EitherNec, ValidatedNec }
import cats.effect._
import cats.syntax.all._

import io.chrisdavenport.cormorant._
import io.chrisdavenport.cormorant.generic.semiauto._
import io.chrisdavenport.cormorant.parser._
import io.chrisdavenport.cormorant.implicits._

import model.execution._
import effects.GenUUID

private[trading] object executing {
  implicit val lr: LabelledRead[ExchangeExecution]  = deriveLabelledRead
  implicit val lw: LabelledWrite[ExchangeExecution] = deriveLabelledWrite

  /** Create executions reading an input stream containing csv data from exchange.
    */
  def createExecutions(
      in: InputStream
  ): IO[ValidatedNec[String, List[IO[Execution]]]] = {
    val acquire = IO {
      scala.io.Source.fromInputStream(in)
    }

    Resource
      .fromAutoCloseable(acquire)
      .use(source => IO(createExecutions[IO](source.mkString)))
  }

  /** Create executions reading a string containing newline separated csv data from exchange.
    */
  def createExecutions[F[_]: Functor: GenUUID](
      exchangeCsv: String
  ): ValidatedNec[String, List[F[Execution]]] = {
    fromExchange(exchangeCsv) match {
      case Left(errs)  => errs.toList.mkString("/").invalidNec
      case Right(eexs) => eexs.traverse(Execution.createExecution[F])
    }
  }

  /** Workhorse method that parses csv data and creates `ExchangeExecution`. No domain validation is done here
    */
  private def fromExchange(
      executions: String
  ): EitherNec[String, List[ExchangeExecution]] = {
    parseComplete(executions)
      .leftWiden[Error]
      .flatMap(_.readLabelled[ExchangeExecution].sequence)
      .toValidatedNec
      .toEither
      .leftMap(_.map(_.toString))
  }
}
