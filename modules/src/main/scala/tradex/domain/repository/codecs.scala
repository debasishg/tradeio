package tradex.domain
package repository

import cats.syntax.all._
import skunk._
import skunk.codec.all._
import squants.market._

import model.user._
import model.account.AccountNo
import model.instrument.ISINCode
import model.order.{OrderNo, UnitPrice, Quantity}
import model.execution.ExecutionReferenceNo
import model.trade.TradeReferenceNo
import model.market.Market
import NewtypeRefinedOps._

object codecs {
  val accountNo: Codec[AccountNo] =
    varchar.eimap[AccountNo] { s =>
      validate[AccountNo](s).leftMap(_.fold)
    }(_.value.value)

  val isinCode: Codec[ISINCode] =
    varchar.eimap[ISINCode] { s =>
      validate[ISINCode](s).leftMap(_.fold)
    }(_.value.value)

  val orderNo: Codec[OrderNo] =
    varchar.eimap[OrderNo] { s =>
      validate[OrderNo](s).leftMap(_.fold)
    }(_.value.value)

  val unitPrice: Codec[UnitPrice] =
    numeric.eimap[UnitPrice] { s =>
      validate[UnitPrice](s).leftMap(_.fold)
    }(_.value.value)

  val quantity: Codec[Quantity] =
    numeric.eimap[Quantity] { s =>
      validate[Quantity](s).leftMap(_.fold)
    }(_.value.value)

  val executionRefNo: Codec[ExecutionReferenceNo] =
    varchar.eimap[ExecutionReferenceNo] { u =>
      validate[ExecutionReferenceNo](u).leftMap(_.fold)
    }(_.value.value)

  val tradeRefNo: Codec[TradeReferenceNo] =
    varchar.eimap[TradeReferenceNo] { u =>
      validate[TradeReferenceNo](u).leftMap(_.fold)
    }(_.value.value)

  val market: Codec[Market] =
    varchar.imap[Market](Market.withName(_))(_.entryName)

  val userId: Codec[UserId] = uuid.imap[UserId](UserId(_))(_.value)

  val userName: Codec[UserName] =
    varchar.eimap[UserName] { u =>
      validate[UserName](u).leftMap(_.fold)
    }(_.value.value)

  val encPassword: Codec[EncryptedPassword] =
    varchar.eimap[EncryptedPassword] { p =>
      validate[EncryptedPassword](p).leftMap(_.fold)
    }(_.value.value)

  val money: Codec[Money] = numeric.imap[Money](USD(_))(_.amount)
}
