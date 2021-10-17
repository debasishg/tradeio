package tradex.domain

import java.util.UUID
import java.time.{ Instant, LocalDateTime }
import cats.effect.IO
import cats.data.NonEmptyList
import model.account._
import model.instrument._
import model.order._
import model.trade._
import model.market._
import model.user._
import http.auth.users.{ User => AuthUser, _ }
import NewtypeRefinedOps._

import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.api.RefType
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import squants.market.USD
import squants.market.JPY

object generators {
  val nonEmptyStringGen: Gen[String] =
    Gen
      .chooseNum(21, 40)
      .flatMap { n =>
        Gen.buildableOfN[String, Char](n, Gen.alphaChar)
      }

  def nesGen[A](f: String => A): Gen[A] =
    nonEmptyStringGen.map(f)

  def idGen[A](f: UUID => A): Gen[A] =
    Gen.uuid.map(f)

  val userIdGen: Gen[UserId] =
    idGen(UserId.apply)

  val userNameGen: Gen[UserName] =
    nesGen(s => UserName.apply(NonEmptyString.unsafeFrom(s)))

  val userGen: Gen[AuthUser] =
    for {
      i <- userIdGen
      n <- userNameGen
    } yield AuthUser(i, n)

  val adminUserGen: Gen[AdminUser] =
    userGen.map(AdminUser(_))

  val commonUserGen: Gen[CommonUser] =
    userGen.map(CommonUser(_))

  val accountNoStringGen: Gen[String] =
    Gen
      .chooseNum(5, 12)
      .flatMap { n =>
        Gen.buildableOfN[String, Char](n, Gen.alphaChar)
      }

  val accountNoGen: Gen[AccountNo] = accountNoStringGen.map(str =>
    validate[AccountNo](str)
      .fold(errs => throw new Exception(errs.toString), identity)
  )

  val accountNameGen: Gen[AccountName] = arbitrary[NonEmptyString].map(AccountName(_))

  val accountNameStringGen: Gen[String] =
    Gen
      .chooseNum(10, 15)
      .flatMap { n =>
        Gen.buildableOfN[String, Char](n, Gen.alphaChar)
      }

  def openCloseDateGen: Gen[(LocalDateTime, LocalDateTime)] = for {
    o <- Gen.oneOf(List(LocalDateTime.now, LocalDateTime.now.plusDays(2)))
    c <- Gen.oneOf(List(LocalDateTime.now.plusDays(10), LocalDateTime.now.plusDays(20)))
  } yield (o, c)

  def tradingAccountGen: Gen[Account] = for {
    no <- accountNoGen
    nm <- accountNameGen
    oc <- openCloseDateGen
    tp <- Gen.const(AccountType.Trading)
    bc <- Gen.const(USD)
    tc <- Gen.oneOf(List(USD, JPY)).map(Some(_))
    sc <- Gen.const(None)
  } yield Account(no, nm, oc._1, Some(oc._2), tp, bc, tc, sc)

  def settlementAccountGen: Gen[Account] = for {
    no <- accountNoGen
    nm <- accountNameGen
    oc <- openCloseDateGen
    tp <- Gen.const(AccountType.Settlement)
    bc <- Gen.const(USD)
    tc <- Gen.const(None)
    sc <- Gen.oneOf(List(USD, JPY)).map(Some(_))
  } yield Account(no, nm, oc._1, Some(oc._2), tp, bc, tc, sc)

  def bothAccountGen: Gen[Account] = for {
    no <- accountNoGen
    nm <- accountNameGen
    oc <- openCloseDateGen
    tp <- Gen.const(AccountType.Both)
    bc <- Gen.const(USD)
    tc <- Gen.oneOf(List(USD, JPY)).map(Some(_))
    sc <- Gen.oneOf(List(USD, JPY)).map(Some(_))
  } yield Account(no, nm, oc._1, Some(oc._2), tp, bc, tc, sc)

  def accountGen: Gen[Account] = Gen.oneOf(tradingAccountGen, settlementAccountGen, bothAccountGen)

  def tradingCreateAccountGen: Gen[CreateAccount] = for {
    no <- accountNoStringGen
    nm <- accountNameStringGen
    oc <- openCloseDateGen
    tp <- Gen.const(AccountType.Trading)
    bc <- Gen.const(USD)
    tc <- Gen.oneOf(List(USD, JPY)).map(Some(_))
    sc <- Gen.const(None)
  } yield CreateAccount(no, nm, Some(oc._1), None, bc, tc, sc, tp)

  def isinGen: Gen[ISINCode] = {
    val appleISINStr = "US0378331005"
    val baeISINStr   = "GB0002634946"
    val ibmISINStr   = "US4592001014"

    val isins = List(appleISINStr, baeISINStr, ibmISINStr)
      .map(str =>
        RefType
          .applyRef[ISINCodeString](str)
          .map(ISINCode(_))
          .fold(err => throw new Exception(err), identity)
      )
    Gen.oneOf(isins)
  }

  def isinNamePairGen: Gen[(ISINCode, InstrumentName)] = {
    val apple = ("US0378331005", "apple")
    val bae   = ("GB0002634946", "bae")
    val ibm   = ("US4592001014", "ibm")

    val isinNames = List(apple, bae, ibm)
      .map(str =>
        (
          RefType
            .applyRef[ISINCodeString](str._1)
            .map(ISINCode(_))
            .fold(err => throw new Exception(err), identity),
          RefType
            .applyRef[NonEmptyString](str._2)
            .map(InstrumentName(_))
            .fold(err => throw new Exception(err), identity)
        )
      )
    Gen.oneOf(isinNames)
  }

  val unitPriceGen: Gen[UnitPrice] = {
    val ups = List(BigDecimal(12.25), BigDecimal(51.25), BigDecimal(55.25))
      .map(n =>
        RefType
          .applyRef[UnitPriceType](n)
          .map(UnitPrice(_))
          .fold(err => throw new Exception(err), identity)
      )
    Gen.oneOf(ups)
  }

  val lotSizeGen: Gen[LotSize] = {
    val ls = List(10, 100)
      .map(n =>
        RefType
          .applyRef[LotSizeType](n)
          .map(LotSize(_))
          .fold(err => throw new Exception(err), identity)
      )
    Gen.oneOf(ls)
  }

  val quantityGen: Gen[Quantity] = {
    val qtys = List(BigDecimal(100), BigDecimal(200), BigDecimal(300))
      .map(n =>
        RefType
          .applyRef[QuantityType](n)
          .map(Quantity(_))
          .fold(err => throw new Exception(err), identity)
      )
    Gen.oneOf(qtys)
  }

  val equityGen: Gen[Instrument] = for {
    isinName <- isinNamePairGen
    up       <- unitPriceGen
    ls       <- lotSizeGen
  } yield Instrument(
    isinName._1,
    isinName._2,
    InstrumentType.Equity,
    Some(LocalDateTime.now),
    None,
    ls,
    Some(up),
    None,
    None
  )

  val tradeGen = for {
    no   <- accountNoGen
    isin <- isinGen
    mkt  <- Gen.oneOf(Market.NewYork, Market.Tokyo, Market.HongKong)
    bs   <- Gen.oneOf(BuySell.Buy, BuySell.Sell)
    up   <- unitPriceGen
    qty  <- quantityGen
    td   <- Gen.oneOf(List(LocalDateTime.now, LocalDateTime.now.plusDays(2)))
    vd   <- Gen.const(None)
  } yield Trade.trade[IO](no, isin, mkt, bs, up, qty, td, vd)

  val tradeForTokyoMarketGen = for {
    no   <- accountNoGen
    isin <- isinGen
    mkt  <- Gen.const(Market.Tokyo)
    bs   <- Gen.oneOf(BuySell.Buy, BuySell.Sell)
    up   <- unitPriceGen
    qty  <- quantityGen
    td   <- Gen.oneOf(List(LocalDateTime.now, LocalDateTime.now.plusDays(2)))
    vd   <- Gen.const(None)
  } yield Trade.trade[IO](no, isin, mkt, bs, up, qty, td, vd)

  val tradeWithTaxFeeGen = for {
    no   <- accountNoGen
    isin <- isinGen
    mkt  <- Gen.oneOf(Market.NewYork, Market.Tokyo, Market.HongKong)
    bs   <- Gen.const(BuySell.Buy)
    up   <- unitPriceGen
    qty  <- quantityGen
    td   <- Gen.oneOf(List(LocalDateTime.now, LocalDateTime.now.plusDays(2)))
    vd   <- Gen.const(None)
  } yield Trade.trade[IO](no, isin, mkt, bs, up, qty, td, vd).map(Trade.withTaxFee)

  def tradeWithTaxFeeForAccountAndInstrumentGen(no: AccountNo, isin: ISINCode) = for {
    mkt <- Gen.oneOf(Market.NewYork, Market.Tokyo, Market.HongKong)
    bs  <- Gen.const(BuySell.Buy)
    up  <- unitPriceGen
    qty <- quantityGen
    td  <- Gen.oneOf(List(LocalDateTime.now, LocalDateTime.now.plusDays(2)))
    vd  <- Gen.const(None)
  } yield Trade.trade[IO](no, isin, mkt, bs, up, qty, td, vd).map(Trade.withTaxFee)

  val frontOfficeOrderGen = for {
    ano  <- accountNoGen
    dt   <- Gen.oneOf(Instant.now, Instant.now.plus(2, java.time.temporal.ChronoUnit.DAYS))
    isin <- isinGen
    qty  <- quantityGen
    up   <- unitPriceGen
    bs   <- Gen.oneOf(BuySell.Buy, BuySell.Sell)
  } yield FrontOfficeOrder(ano.value.value, dt, isin.value.value, qty.value.value, up.value.value, bs.entryName)

  val generateTradeFrontOfficeInputGen = for {
    orders         <- Gen.nonEmptyListOf(frontOfficeOrderGen)
    mkt            <- Gen.oneOf(Market.NewYork, Market.Tokyo, Market.HongKong)
    brkAccountNo   <- accountNoGen
    clientAccounts <- Gen.nonEmptyListOf(accountNoGen)
  } yield GenerateTradeFrontOfficeInput(
    NonEmptyList.fromListUnsafe(orders),
    mkt,
    brkAccountNo,
    NonEmptyList.fromListUnsafe(clientAccounts)
  )
}
