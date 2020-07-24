package tradex.domain
package trading

import java.io.InputStream

import cats.effect._
import cats.implicits._
import cats.data.NonEmptyList

import io.chrisdavenport.cormorant._
import io.chrisdavenport.cormorant.generic.semiauto._
import io.chrisdavenport.cormorant.parser._
import io.chrisdavenport.cormorant.implicits._

import common._
import model.order._
import Order._

object ordering {
  implicit val lr: LabelledRead[FrontOfficeOrder] = deriveLabelledRead
  implicit val lw: LabelledWrite[FrontOfficeOrder] = deriveLabelledWrite

  /**
    * Create orders reading an input stream containing csv data from
    * front office.
    * The format is as follows:
    * accountNo,date,isin,qty,buySell
    */
  def createOrders(in: InputStream): IO[ErrorOr[List[Order]]] = {
    val acquire = IO {
      scala.io.Source.fromInputStream(in)
    }

    Resource
      .fromAutoCloseable(acquire)
      .use(source => IO(createOrders(source.mkString)))
  }

  /**
    * Create orders reading a string containing newline separated csv data from
    * front office.
    * The format is as follows:
    * accountNo,date,isin,qty,buySell
    */
  def createOrders(frontOfficeCsv: String): ErrorOr[List[Order]] =
    fromFrontOffice(frontOfficeCsv).flatMap(create)

  /**
    * Workhorse method that parses csv data and creates `FrontOfficeOrder`.
    * No domain validation is done here
    */
  private def fromFrontOffice(
      order: String
  ): ErrorOr[NonEmptyList[FrontOfficeOrder]] = {
    parseComplete(order)
      .leftWiden[Error]
      .flatMap(_.readLabelled[FrontOfficeOrder].sequence)
      .toValidatedNec
      .toEither
      .leftMap(_.map(_.toString))
      .map(l => NonEmptyList.fromList(l).get)
  }
}
