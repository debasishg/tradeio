package org.github.domain
package repository
package interpreter.skunk

import java.time.LocalDateTime

import cats.Semigroup
import cats.data.NonEmptyList
import cats.implicits._
import cats.effect._

import skunk._
import skunk.data.Type
import skunk.codec.all._
import skunk.implicits._

import squants.market._

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
            else lines.tail.foldLeft(lines.head)(Semigroup[Order].combine).some
          }
      }
    }

  private def makeSingleLineItemOrders(ono: OrderNo, 
    lis: List[LocalDateTime ~ String ~ String ~ BigDecimal ~ BigDecimal ~ BuySell ~ String]): List[Order] = {

    lis.map {
      case odt ~ ano ~ isin ~ qty ~ up ~ bs ~ ono => 
        Order(
          OrderNo(ono), 
          odt, 
          AccountNo(ano), 
          NonEmptyList.one(
            LineItem(ISINCode(isin), Quantity(qty), UnitPrice(up), bs)
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
                singleOrders.tail.foldLeft(singleOrders.head)(Semigroup[Order].combine)
            }.toList
          }
      }
    }

    /**
      * Generic store API that handles both inserts and updates. The steps to be followed are:
      * 
      * 1. delete line-items (if any) corresponding to this order number
      * 2. upsert order record
      * 3. insert new line-items
      *
      * @param ord the order to store
      * @return the order with an effect
      */
  def store(ord: Order): M[Order] = 
    sessionPool.use { session =>
      session.transaction.use { _ =>
        storeOrderAndLineItems(ord, session)
      }
    }

  // prepareAndExecute is an extension method defined in skunkx
  private def storeOrderAndLineItems(ord: Order, session: Session[M]): M[Order] = {
    session.prepareAndExecute(deleteLineItems, ord.no.value) *>
    session.prepareAndExecute(upsertOrder, ord.no.value ~ ord.date ~ ord.accountNo.value) *>
    session.prepareAndExecute(insertLineItems(ord.items.size), ord.items.toList).void.map(_ => ord)
  }

  def store(orders: NonEmptyList[Order]): M[Unit] =
    sessionPool.use { session =>
      session.transaction.use { xa =>
        orders.toList.map { ord =>
          storeOrderAndLineItems(ord, session)
        }.sequence.map(_ => ())
      } 
    } 
}
  
private object OrderQueries {

  val buySell = enum(BuySell, Type("buysellflag"))

  val orderLineItemDecoder = timestamp ~ varchar ~ varchar ~ numeric ~ numeric ~ buySell ~ varchar

  val orderEncoder: Encoder[Order] = 
    (varchar ~ varchar ~ timestamp).values.contramap((o: Order) => o.no.value ~ o.accountNo.value ~ o.date)

  val lineItemEncoder: Encoder[LineItem] =
    (varchar ~ numeric ~ numeric ~ varchar).values.contramap((li: LineItem) => 
      li.instrument.value ~ li.quantity.value ~ li.unitPrice.value ~ li.buySell.toString)

  val selectByOrderNo =  
    sql"""
        SELECT o.dateOfOrder, o.accountNo, l.isinCode, l.quantity, l.unitPrice, l.buySellFlag, o.no
        FROM orders o, lineItems l
        WHERE o.no = ${varchar.cimap[OrderNo]}
        AND   o.no = l.orderNo
       """.query(orderLineItemDecoder)

  val selectByOrderDate =  
    sql"""
        SELECT o.dateOfOrder, o.accountNo, l.isinCode, l.quantity, l.unitPrice, l.buySellFlag, o.no
        FROM orders o, lineItems l
        WHERE o.dateOfOrder = ${timestamp}
        AND   o.no = l.orderNo
       """.query(orderLineItemDecoder)

  val insertOrder: Command[Order] =
    sql"INSERT INTO orders (no, dateOfOrder, accountNo) VALUES $orderEncoder".command

  val insertLineItem: Command[LineItem] =
    sql"INSERT INTO lineItems (orderNo, isinCode, quantity, unitPrice, buySellFlag) VALUES $lineItemEncoder".command

  def insertLineItems(n: Int): Command[List[LineItem]] = {
    val es = lineItemEncoder.list(n)
    sql"INSERT INTO lineItems (orderNo, isinCode, quantity, unitPrice, buySellFlag) VALUES $es".command
  }

  val upsertOrder =  
    sql"""
        INSERT INTO orders
        VALUES ($varchar, $timestamp, $varchar)
        ON CONFLICT(no) DO UPDATE SET
          dateOfOrder = EXCLUDED.dateOfOrder,
          accountNo   = EXCLUDED.accountNo
       """.command

  val deleteLineItems: Command[String] =
    sql"DELETE FROM lineItems WHERE orderNo = $varchar".command
}

// Smart constructor 
object OrderRepositoryInterpreter {
  def make[M[_]: Sync](
    sessionPool: Resource[M, Session[M]]
  ): M[OrderRepositoryInterpreter[M]] = Sync[M].delay(new OrderRepositoryInterpreter[M](sessionPool))
}
