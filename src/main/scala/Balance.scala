import argonaut._
import Argonaut._

import monix.execution.Scheduler.Implicits.global

import scala.concurrent.Future

object CoinMarketCap {
  def getKeyFromTicker(cName: String, key: String): Future[String] = {
    val request = FakeBrowserHttpRequest(s"https://api.coinmarketcap.com/v1/ticker/$cName/")
    request.send().map { response =>
      val parsedResponse = response.body.parseOption
      if (parsedResponse.isDefined && parsedResponse.get.arrayOrEmpty.nonEmpty) {
        parsedResponse.get.array.get.head.fieldOrZero(key).as[String].getOr("0")
      }
      else "0"
    }
  }
}

object FixerIo {
  def getCurrencyXchgRate(base: String, cName: String): Future[Double] = {
    val request = FakeBrowserHttpRequest("https://api.fixer.io/latest").withQueryParameter("base", base)
    request.send().map { response =>
      val parsedResponse = response.body.parseOption
      if (parsedResponse.isDefined && parsedResponse.get.hasField("rates")) {
        parsedResponse.get.fieldOrEmptyObject("rates").fieldOrZero(cName).as[Double].getOr(0.0)
      }
      else 0
    }
  }
}

case class Balance(value: Double, currency: Currency) {
  // TODO: doesn't work when the current balance is in fiat
  def inBTC: Future[Balance] = CoinMarketCap.getKeyFromTicker(currency.name, "price_btc").map(r => Balance(r.toDouble * value, BTC))
  def inUSD: Future[Balance] = CoinMarketCap.getKeyFromTicker(currency.name, "price_usd").map(r => Balance(r.toDouble * value, USD))
  def inCZK: Future[Balance] = {
    for {
      balance <- currency match {
        case USD => Future(this)
        case _ => this.inUSD
      }
      rate <- FixerIo.getCurrencyXchgRate("USD", "CZK")
    } yield Balance(rate * balance.value, CZK)
  }

  override def toString: String = {
    val cSymbol =
      if (currency.symbol != null) currency.symbol
      else currency.getClass.getSimpleName.dropRight(1)

    val isFiat = currency match {
      case c: Fiat => true
      case _ => false
    }

    if (currency.prefixNotation) f"$cSymbol$value%.2f"
    else if (isFiat) f"$value%.2f $cSymbol"
    else f"$value%.8f $cSymbol"
  }
}
