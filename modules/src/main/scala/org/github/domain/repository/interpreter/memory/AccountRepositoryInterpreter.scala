package org.github.domain
package repository
package interpreter
package memory

import java.time.LocalDateTime
import scala.collection.immutable.Map 

import cats._
import cats.implicits._
import cats.effect.concurrent.Ref
import cats.effect.Sync

import model.account._
import model.newtypes._
import model.enums._

// Constructor private for the interpreter to prevent the Ref from leaking
// access through smart constructor below
final class AccountRepositoryInterpreter[M[_]: Monad] private (repo: Ref[M, Map[AccountNo, Account]])
  extends AccountRepository[M] {

  def query(no: AccountNo): M[Option[Account]] = repo.get.map(_.get(no))  

  def store(a: Account): M[Account] = repo.update(_ + ((a.no, a))).map(_ => a)

  def query(openedOn: LocalDateTime): M[List[Account]] = 
    repo.get.map(_.values.filter(_.dateOfOpen == openedOn).toList)

  def all: M[List[Account]] = repo.get.map(_.values.toList)

  def allClosed(closeDate: Option[LocalDateTime]): M[List[Account]] = 
    repo.get.map(_.values.filter(_.dateOfClose.isDefined).toList)

  def allAccountsOfType(accountType: AccountType): M[List[Account]] = 
    repo.get.map(_.values.filter(_.accountType == accountType).toList)
}

// Smart constructor 
object AccountRepositoryInterpreter {
  def make[M[_]: Sync]: M[AccountRepositoryInterpreter[M]] =
    Ref.of[M, Map[AccountNo, Account]](Map.empty).map(new AccountRepositoryInterpreter(_))
}