package org.github.domain
package repository
package interpreter.skunk

import java.time.LocalDateTime

import cats.data.NonEmptyList
import cats.implicits._
import cats.effect._
import cats.effect.concurrent.Ref

import skunk._
import skunk.data.Type
import skunk.codec.all._
import skunk.implicits._

import squants.market._
import squants.market.defaultMoneyContext

import model.newtypes._
import model.enums._
import model.order._
import Order._
import ext.skunkx._

final class OrderRepositoryInterpreter[M[_]: Sync] private (
  sessionPool: Resource[M, Session[M]]) extends OrderRepository[M] {

  import OrderQueries._

  def query(no: OrderNo): M[Option[Order]] = 
    sessionPool.use { session =>
      session.prepare(selectByOrderNo).use { ps =>
        ps.stream(no, 1024)
          .compile
          .toList
          .map(_.groupBy(_._2)).map { m =>
            val lines: List[Order] = m.map {
              case (ono, lis) => makeSingleLineItemOrders(no, lis)
            }.head
            if (lines.isEmpty) None
            else lines.tail.foldLeft(lines.head)((a, e) => a.combine(e)).some
          }
      }
    }

  private def makeSingleLineItemOrders(ono: OrderNo, 
    lis: List[LocalDateTime ~ String ~ String ~ BigDecimal ~ BuySell ~ String]): List[Order] = {

    lis.map {
      case odt ~ ano ~ isin ~ qty ~ bs ~ ono => 
        Order(
          OrderNo(ono), 
          odt, 
          AccountNo(ano), 
          NonEmptyList.one(
            LineItem(ISINCode(isin), Quantity(qty), UnitPrice(BigDecimal.decimal(1.0)), bs)
          )
        )
    }
  }

  def queryByOrderDate(date: LocalDateTime): M[List[Order]] = 
    sessionPool.use { session =>
      session.prepare(selectByOrderDate).use { ps =>
        ps.stream(date, 1024)
          .compile
          .toList
          .map(_.groupBy(_._2)).map { m =>
            m.map {
              case (ono, lis) => 
                val singleOrders = makeSingleLineItemOrders(OrderNo(ono), lis)
                singleOrders.tail.foldLeft(singleOrders.head)((a, e) => a.combine(e))
            }.toList
          }
      }
    }

  def store(ord: Order): M[Order] = 
    sessionPool.use { session =>
      session.transaction.use { xa =>
        session.prepare(insertOrder).use { cmd =>
          cmd.execute(ord)
        } *>
        session.prepare(insertLineItems(ord.items.size)).use { cmd =>
          cmd.execute(ord.items.toList).void.map(_ => ord)
        } 
      }
    }

  def store(orders: NonEmptyList[Order]): M[Unit] = ???

}
  
private object OrderQueries {

  // A codec that maps Postgres type `accountType` to Scala type `AccountType`
  val buySell = enum(BuySell, Type("buysellflag"))
  implicit val moneyContext = defaultMoneyContext

  val orderLineItemDecoder = timestamp ~ varchar ~ varchar ~ numeric ~ buySell ~ varchar

  val orderEncoder: Encoder[Order] = 
    (varchar ~ varchar ~ timestamp).values.contramap((o: Order) => o.no.value ~ o.accountNo.value ~ o.date)

  val lineItemEncoder: Encoder[LineItem] =
    (varchar ~ numeric ~ numeric ~ varchar).values.contramap((li: LineItem) => 
      li.instrument.value ~ li.quantity.value ~ li.unitPrice.value ~ li.buySell.toString)

  val selectByOrderNo =  // : Query[OrderNo, Order] =
    sql"""
        SELECT o.dateOfOrder, o.accountNo, l.isinCode, l.quantity, l.buySellFlag, o.no
        FROM orders o, lineItems l
        WHERE o.no = ${varchar.cimap[OrderNo]}
        AND   o.no = l.orderNo
       """.query(orderLineItemDecoder)

  val selectByOrderDate =  // : Query[LocalDateTime, Order] =
    sql"""
        SELECT o.dateOfOrder, o.accountNo, l.isinCode, l.quantity, l.buySellFlag, o.no
        FROM orders o, lineItems l
        WHERE o.dateOfOrder = ${timestamp}
        AND   o.no = l.orderNo
       """.query(orderLineItemDecoder)

  val insertOrder: Command[Order] =
    sql"INSERT INTO orders (no, dateOfOrder, accountNo) VALUES $orderEncoder".command

  val insertLineItem: Command[LineItem] =
    sql"INSERT INTO lineItems (orderNo, isinCode, quantity, buySellFlag) VALUES $lineItemEncoder".command

  def insertLineItems(n: Int): Command[List[LineItem]] = {
    val es = lineItemEncoder.list(n)
    sql"INSERT INTO lineItems (orderNo, isinCode, quantity, buySellFlag) VALUES $es".command
  }
}