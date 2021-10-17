package tradex.domain
package http.routes.admin

import cats.data.Kleisli
import cats.effect._
import cats.syntax.all._
import model.instrument._
import io.circe.JsonObject
import io.circe.syntax._

import org.http4s.Method._
import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.client.dsl.io._
import org.http4s.server.AuthMiddleware
import org.http4s.syntax.literals._

import suite.HttpSuite
import generators._
import http.auth.users._
import repository.InstrumentRepository

object AdminInstrumentRoutesSuite extends HttpSuite {
  def authMiddleware(authUser: AdminUser): AuthMiddleware[IO, AdminUser] =
    AuthMiddleware(Kleisli.pure(authUser))

  test("POST instrument succeeds") {
    val gen = for {
      equity <- createEquityGen
      user   <- adminUserGen
    } yield (equity, user)

    forall(gen) { case (createInstrument, user) =>
      val req      = POST(createInstrument, uri"/instruments")
      val routes   = AdminInstrumentRoutes[IO](new TestInstrumentRepository).routes(authMiddleware(user))
      val expected = JsonObject.singleton("instrument", createInstrument.toDomain.toOption.get.asJson).asJson
      expectHttpBodyAndStatus(routes, req)(expected, Status.Created)
    }
  }
}

protected class TestInstrumentRepository extends InstrumentRepository[IO] {
  def query(isin: ISINCode): IO[Option[Instrument]]                               = IO.pure(none[Instrument])
  def queryByInstrumentType(instrumentType: InstrumentType): IO[List[Instrument]] = IO.pure(List.empty[Instrument])
  def store(ins: Instrument): IO[Instrument]                                      = IO.pure(ins)
}
