package tradex.domain
package services.trading

import java.io.InputStream

import cats.effect._
import cats.syntax.all._
import cats.data.{ EitherNec, NonEmptyList }

import io.chrisdavenport.cormorant._
import io.chrisdavenport.cormorant.generic.semiauto._
import io.chrisdavenport.cormorant.parser._
import io.chrisdavenport.cormorant.implicits._

import model.order._
import Order._

private[trading] object ordering {
  implicit val lr: LabelledRead[FrontOfficeOrder]  = deriveLabelledRead
  implicit val lw: LabelledWrite[FrontOfficeOrder] = deriveLabelledWrite

  /** Create orders reading an input stream containing csv data from
    * front office.
    * The format is as follows:
    * accountNo,date,isin,qty,buySell
    */
  def createOrders(in: InputStream): IO[EitherNec[String, List[Order]]] = {
    val acquire = IO {
      scala.io.Source.fromInputStream(in)
    }

    Resource
      .fromAutoCloseable(acquire)
      .use(source => IO(createOrders(source.mkString)))
  }

  /** Create orders reading a string containing newline separated csv data from
    * front office.
    * The format is as follows:
    * accountNo,date,isin,qty,buySell
    */
  def createOrders(frontOfficeCsv: String): EitherNec[String, List[Order]] =
    fromFrontOffice(frontOfficeCsv).flatMap(create)

  /** Workhorse method that parses csv data and creates `FrontOfficeOrder`.
    * No domain validation is done here
    */
  private def fromFrontOffice(
      order: String
  ): EitherNec[String, NonEmptyList[FrontOfficeOrder]] = {
    parseComplete(order)
      .leftWiden[Error]
      .flatMap(_.readLabelled[FrontOfficeOrder].sequence)
      .toValidatedNec
      .toEither
      .leftMap(_.map(_.toString))
      .map(l => NonEmptyList.fromList(l).get)
  }
}
