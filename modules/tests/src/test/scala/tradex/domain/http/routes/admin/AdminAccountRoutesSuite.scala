package tradex.domain
package http.routes.admin

import org.scalacheck.Gen
import cats.data.Kleisli
import cats.effect._

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
import http.routes.TestAccountRepository
import http.auth.users._
import cats.data.NonEmptyList
import tradex.domain.model.account

object AdminAccountRoutesSuite extends HttpSuite {
  def authMiddleware(authUser: AdminUser): AuthMiddleware[IO, AdminUser] =
    AuthMiddleware(Kleisli.pure(authUser))

  final val err = "Unique constraint violation: Attempt to store duplicate account number"

  class TestAccountRepositoryWithFailedStore extends TestAccountRepository {
    override def store(as: NonEmptyList[account.Account]): IO[Unit] =
      IO.raiseError(new Exception(err))
  }

  test("POST accounts succeeds") {
    val gen = for {
      cp <- Gen.listOfN(5, tradingCreateAccountGen)
      au <- adminUserGen
    } yield (cp, au)

    forall(gen) { case (createAccounts, user) =>
      val req      = POST(createAccounts, uri"/accounts")
      val routes   = AdminAccountRoutes[IO](new TestAccountRepository).routes(authMiddleware(user))
      val expected = JsonObject.singleton("accounts", "Ok".asJson).asJson
      expectHttpBodyAndStatus(routes, req)(expected, Status.Created)
    }
  }

  test("POST accounts fails") {
    val gen = for {
      cp <- Gen.listOfN(5, tradingCreateAccountGen)
      au <- adminUserGen
    } yield (cp, au)

    forall(gen) { case (createAccounts, user) =>
      val req      = POST(createAccounts, uri"/accounts")
      val routes   = AdminAccountRoutes[IO](new TestAccountRepositoryWithFailedStore).routes(authMiddleware(user))
      val expected = err.asJson
      expectHttpBodyAndStatus(routes, req)(expected, Status.InternalServerError)
    }
  }
}
