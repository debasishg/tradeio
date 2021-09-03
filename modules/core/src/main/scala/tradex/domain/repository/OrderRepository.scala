package tradex.domain
package repository

import java.time.LocalDate

import cats.Semigroup
import cats.data.NonEmptyList
import cats.syntax.all._
import cats.effect._

import skunk._
import skunk.data.Type
import skunk.codec.all._
import skunk.implicits._

import model.order._
import codecs._

trait OrderRepository[F[_]] {

  /** query by unique key order no, account number and date */
  def query(no: OrderNo): F[Option[Order]]

  /** query by order date */
  def queryByOrderDate(date: LocalDate): F[List[Order]]

  /** store */
  def store(ord: Order): F[Order]

  /** store many orders */
  def store(orders: NonEmptyList[Order]): F[Unit]
}

object OrderRepository {
  def make[F[_]: Concurrent](
      postgres: Resource[F, Session[F]]
  ): OrderRepository[F] =
    new OrderRepository[F] {
      import OrderRepositorySQL._

      // semigroup that combines orders with same order number
      // used in combining join records between orders and lineItems tables
      // NOT a generic semigroup that combines all orders - only specific
      // to this query - hence not added in the companion object
      implicit val orderConcatSemigroup: Semigroup[Order] =
        new Semigroup[Order] {
          def combine(x: Order, y: Order): Order =
            Order(x.no, x.date, x.accountNo, x.items ++ y.items.toList)
        }

      def query(no: OrderNo): F[Option[Order]] =
        postgres.use { session =>
          session.prepare(selectByOrderNo).use { ps =>
            ps.stream(no, 1024)
              .compile
              .toList
              .map(_.groupBy(_.no))
              .map {
                _.map { case (_, lis) =>
                  lis.reduce(Semigroup[Order].combine)
                }.headOption
              }
          }
        }

      def queryByOrderDate(date: LocalDate): F[List[Order]] =
        postgres.use { session =>
          session.prepare(selectByOrderDate).use { ps =>
            ps.stream(date, 1024)
              .compile
              .toList
              .map(_.groupBy(_.no))
              .map { m =>
                m.map { case (_, lis) =>
                  lis.reduce(Semigroup[Order].combine)
                }.toList
              }
          }
        }

      /** Generic store API that handles both inserts and updates. The steps to be followed are:
        *
        * 1. delete line-items (if any) corresponding to this order number
        * 2. upsert order record
        * 3. insert new line-items
        *
        * @param ord the order to store
        * @return the order with an effect
        */
      def store(ord: Order): F[Order] =
        postgres.use { session =>
          session.transaction.use { _ =>
            storeOrderAndLineItems(ord, session)
          }
        }

      private def storeOrderAndLineItems(
          ord: Order,
          session: Session[F]
      ): F[Order] = {
        val lineItems = ord.items.toList
        session.prepare(deleteLineItems).use(_.execute(ord.no.value.value)) *>
          session
            .prepare(upsertOrder)
            .use(
              _.execute(
                ord.no.value.value ~ ord.date ~ ord.accountNo.value.value
              )
            ) *>
          session
            .prepare(insertLineItems(ord.no, lineItems))
            .use { cmd =>
              cmd.execute(lineItems)
            }
            .void
            .map(_ => ord)
      }

      def store(orders: NonEmptyList[Order]): F[Unit] =
        postgres.use { session =>
          session.transaction.use { _ =>
            orders.toList
              .map { ord =>
                storeOrderAndLineItems(ord, session)
              }
              .sequence
              .map(_ => ())
          }
        }
    }
}

private object OrderRepositorySQL {
  val buySell = enum(BuySell, Type("buysell"))

  val orderLineItemDecoder: Decoder[Order] =
    (timestamp ~ accountNo ~ isinCode ~ quantity ~ unitPrice ~ buySell ~ orderNo)
      .map { case od ~ ano ~ isin ~ qty ~ up ~ bs ~ ono =>
        Order(ono, od, ano, NonEmptyList.of(LineItem(ono, isin, qty, up, bs)))
      }

  val orderEncoder: Encoder[Order] =
    (orderNo ~ accountNo ~ timestamp).values
      .contramap((o: Order) => o.no ~ o.accountNo ~ o.date)

  def lineItemEncoder(ordNo: OrderNo) =
    (orderNo ~ isinCode ~ quantity ~ unitPrice ~ buySell).values
      .contramap((li: LineItem) => ordNo ~ li.instrument ~ li.quantity ~ li.unitPrice ~ li.buySell)

  val selectByOrderNo =
    sql"""
        SELECT o.dateOfOrder, o.accountNo, l.isinCode, l.quantity, l.unitPrice, l.buySellFlag, o.no
        FROM orders o, lineItems l
        WHERE o.no = $orderNo
        AND   o.no = l.orderNo
       """.query(orderLineItemDecoder)

  val selectByOrderDate =
    sql"""
        SELECT o.dateOfOrder, o.accountNo, l.isinCode, l.quantity, l.unitPrice, l.buySellFlag, o.no
        FROM orders o, lineItems l
        WHERE Date(o.dateOfOrder) = $date
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

  def insertLineItems(
      orderNo: OrderNo,
      lineItems: List[LineItem]
  ): Command[lineItems.type] = {
    val es = lineItemEncoder(orderNo).list(lineItems)
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
