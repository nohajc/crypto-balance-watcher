import argonaut._
import Argonaut._

import scala.concurrent.Future
import fr.hmil.roshttp.HttpRequest
import monix.execution.Scheduler.Implicits.global


trait BalanceSourceImpl {
  def getCurrent(arg: String): Future[Double]
}

object BalanceSourceImpl {
  def isBitPayInsightAPI(host: String): Boolean = {
    host.contains("bitcore2.trezor.io") ||
      host == "insight.bitpay.com" ||
      host == "cashexplorer.bitcoin.com" ||
      host == "insight.litecore.io"
  }

  def fromHost(host: String): BalanceSourceImpl = host match {
    case host1 if isBitPayInsightAPI(host1) => new BitPayBalanceSourceImpl(host1)
    case host2 if host2 == "api.etherscan.io" => new EtherScanBalanceSourceImpl(host2)
    case host3 if host3 == "etcchain.com" => new EtcChainBalanceSourceImpl(host3)
    case host4 if host4 == "chain.so" => new ChainSoBalanceSourceImpl(host4)
  }
}

// TODO: handle HTTP exceptions, retry

class BitPayBalanceSourceImpl(host: String) extends BalanceSourceImpl {
  override def getCurrent(addr: String): Future[Double] = {
    val request = FakeBrowserHttpRequest(s"https://$host/api/addr/$addr/utxo")
    request.send().map({ response =>
      val parsedResponse = response.body.parseOption
      if (parsedResponse.isDefined && parsedResponse.get.isArray) {
        parsedResponse.get.arrayOrEmpty.map(_.fieldOrZero("amount").as[Double].getOr(0.0)).sum
      }
      else 0
    })
  }
}

class ChainSoBalanceSourceImpl(host: String) extends BalanceSourceImpl {
  override def getCurrent(addr: String): Future[Double] = {
    val url = s"https://$host/api/v2/get_tx_unspent/ltc/${BipUtils.convertToLTC3Address(addr)}"
    val request = FakeBrowserHttpRequest(url)
    request.send().map({ response =>
      val parsedResponse = response.body.parseOption
      if (parsedResponse.isDefined) {
       val data = parsedResponse.get.fieldOrEmptyObject("data")
       data.fieldOrEmptyObject("txs").arrayOrEmpty.map(_.fieldOrZero("value").as[Double].getOr(0.0)).sum
      }
      else 0
    })
  }
}

class EtherScanBalanceSourceImpl(host: String) extends BalanceSourceImpl {
  val APIKey: String = Secret.get[String]("EtherScanAPIKey").getOrElse("")

  override def getCurrent(addr: String): Future[Double] = {
    val request = FakeBrowserHttpRequest(s"https://$host/api")
      .withQueryParameter("module", "account")
      .withQueryParameter("action", "balance")
      .withQueryParameter("address", addr)
      .withQueryParameter("tag", "latest")
      .withQueryParameter("apikey", APIKey)

    request.send().map({ response =>
      val parsedResponse = response.body.parseOption
      if (parsedResponse.isDefined && parsedResponse.get.fieldOrEmptyString("message").as[String].getOr("") == "OK") {
        parsedResponse.get.fieldOrZero("result").as[Double].getOr(0.0) / 1E18
      }
      else 0
    })
  }
}

class EtcChainBalanceSourceImpl(host: String) extends BalanceSourceImpl {
  override def getCurrent(addr: String): Future[Double] = {
    val request = FakeBrowserHttpRequest(s"https://$host/api/v1/getAddressBalance")
      .withQueryParameter("address", addr)

    request.send().map({ response =>
      val parsedResponse = response.body.parseOption
      if (parsedResponse.isDefined) {
        parsedResponse.get.fieldOrZero("balance").as[Double].getOr(0.0)
      }
      else 0
    })
  }
}
