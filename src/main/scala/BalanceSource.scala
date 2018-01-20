import argonaut._
import Argonaut._
import crypto.{HMAC, SHA256}
import utils.HexUtils._

import scala.concurrent.Future
import monix.execution.Scheduler.Implicits.global

trait BalanceSource {
  def getCurrent(c: Currency = Default): Future[Double]
  // TODO getCurrent - multiple currencies
}

object BalanceSource {
  def fromHost(host: String, credentials: Credentials = Credentials.empty): BalanceSource = host match {
    case host1 if host1 == "api.binance.com" => new BinanceBalanceSource(host1, credentials)
  }
}

class AddressBalanceSource(addr: String, srcImpl: BalanceSourceImpl) extends BalanceSource {
  override def getCurrent(c: Currency): Future[Double] = srcImpl.getCurrent(addr)
}

class HDWalletBalanceSource(addresses: Stream[String], srcImpl: BalanceSourceImpl) extends BalanceSource {
  // TODO: cache previous results
  def scanAddressesFrom(addresses: Stream[String], i: Int, acc: Double = 0): Future[Double] = {
    val addr = addresses(i)
    new AddressBalanceSource(addr, srcImpl).getCurrent().flatMap { balance =>
      if (balance != 0)
        scanAddressesFrom(addresses, i + 1, acc + balance)
      else Future(acc)
    }
  }

  override def getCurrent(c: Currency): Future[Double] = scanAddressesFrom(addresses, 0)
}

class BinanceBalanceSource(host: String, credentials: Credentials) extends BalanceSource {
  private val APIKey: String = credentials.key
  private val APISecret: String = credentials.secret

  override def getCurrent(c: Currency): Future[Double] = {
    val reqBody = s"timestamp=${System.currentTimeMillis}"
    val signature = HMAC(SHA256)(APISecret.getBytes, reqBody.getBytes).toHexString
    val url = s"https://$host/api/v3/account?$reqBody&signature=$signature"

    val request = FakeBrowserHttpRequest(url).withHeader("X-MBX-APIKEY", APIKey)
    request.send().map { response =>
      val parsedResponse = response.body.parseOption

      if (parsedResponse.isDefined) {
        val balances = parsedResponse.get.fieldOrEmptyArray("balances").arrayOrEmpty
        balances.find { elem =>
          val asset = elem.fieldOrEmptyString("asset").toString
          c.short.r.findFirstIn(asset).isDefined
        }.getOrElse(Json.jEmptyObject).fieldOrEmptyString("free").as[Double].getOr(0.0)
      }
      else 0
    }
  }
}

class ConstantBalanceSource(balance: Double) extends BalanceSource {
  override def getCurrent(c: Currency): Future[Double] = Future(balance)
}