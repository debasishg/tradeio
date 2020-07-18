package tradex.domain
package repository
package interpreter.skunk

import java.time.LocalDateTime

import cats.{ Semigroup, Foldable }
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
    sessionPool: Resource[M, Session[M]]
) extends OrderRepository[M] {

  import OrderQueries._

  // semigroup that combines orders with same order number
  // used in combining join records between orders and lineItems tables
  // NOT a generic semigroup that combines all orders - only specific
  // to this query - hence not added in the companion object
  implicit val orderConcatSemigroup: Semigroup[Order] = new Semigroup[Order] {
    def combine(x: Order, y: Order): Order =
      Order(x.no, x.date, x.accountNo, x.items ++ y.items.toList)
  }

  def query(no: OrderNo): M[Option[Order]] =
    sessionPool.use { session =>
      session.prepare(selectByOrderNo).use { ps =>
        ps.stream(no, 1024)
          .compile
          .toList
          .map(_.groupBy(_._2))
          .map { m =>
            val lines: List[Order] = m.map {
              case (ono, lis) => makeSingleLineItemOrders(no, lis)
            }.head
            combineSingleLineItemOrders(lines)
          }
      }
    }

  /**
    * Takes one joined record which has the `Order` and one `LineItem`
    * and returns an `Order` with a single `LineItem` in it.
    */
  private def makeSingleLineItemOrders(
      ono: OrderNo,
      lis: List[
        LocalDateTime ~ String ~ String ~ BigDecimal ~ BigDecimal ~ BuySell ~ String
      ]
  ): List[Order] = {
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

  private def combineSingleLineItemOrders(orders: List[Order]): Option[Order] = 
    if (orders.isEmpty) None
    else orders.tail.foldLeft(orders.head)(Semigroup[Order].combine).some

  def queryByOrderDate(date: LocalDateTime): M[List[Order]] =
    sessionPool.use { session =>
      session.prepare(selectByOrderDate).use { ps =>
        ps.stream(date, 1024)
          .compile
          .toList
          .map(_.groupBy(_._2))
          .map { m =>
            m.map {
              case (ono, lis) =>
                val singleOrders = makeSingleLineItemOrders(OrderNo(ono), lis)
                singleOrders.tail
                  .foldLeft(singleOrders.head)(Semigroup[Order].combine)
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
  private def storeOrderAndLineItems(
      ord: Order,
      session: Session[M]
  ): M[Order] = {
    session.prepareAndExecute(deleteLineItems, ord.no.value) *>
      session.prepareAndExecute(
        upsertOrder,
        ord.no.value ~ ord.date ~ ord.accountNo.value
      ) *>
      session
        .prepareAndExecute(
          insertLineItems(ord.no, ord.items.size),
          ord.items.toList
        )
        .void
        .map(_ => ord)
  }

  def store(orders: NonEmptyList[Order]): M[Unit] =
    sessionPool.use { session =>
      session.transaction.use { xa =>
        orders.toList
          .map { ord =>
            storeOrderAndLineItems(ord, session)
          }
          .sequence
          .map(_ => ())
      }
    }
}

private object OrderQueries {
  val buySell = enum(BuySell, Type("buysell"))

  val orderLineItemDecoder = timestamp ~ varchar ~ varchar ~ numeric ~ numeric ~ buySell ~ varchar

  val orderEncoder: Encoder[Order] =
    (varchar ~ varchar ~ timestamp).values
      .contramap((o: Order) => o.no.value ~ o.accountNo.value ~ o.date)

  def lineItemEncoder(orderNo: OrderNo) =
    (varchar ~ varchar ~ numeric ~ numeric ~ buySell).values.contramap(
      (li: LineItem) =>
        orderNo.value ~ li.instrument.value ~ li.quantity.value ~ li.unitPrice.value ~ li.buySell
    )

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

  def insertLineItem(orderNo: OrderNo): Command[LineItem] =
    sql"INSERT INTO lineItems (orderNo, isinCode, quantity, unitPrice, buySellFlag) VALUES ${lineItemEncoder(orderNo)}".command

  def insertLineItems(orderNo: OrderNo, n: Int): Command[List[LineItem]] = {
    val es = lineItemEncoder(orderNo).list(n)
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
  ): M[OrderRepositoryInterpreter[M]] =
    Sync[M].delay(new OrderRepositoryInterpreter[M](sessionPool))
}
