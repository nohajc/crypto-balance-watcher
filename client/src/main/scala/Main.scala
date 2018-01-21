import hdwallet.{ScalaXpub, Xpub}
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

import org.scalajs.jquery.jQuery
import org.scalajs.dom.window.localStorage

object Main {
  Xpub.selectImplementation(ScalaXpub)

  def loadConfig(): Unit = {
    //if (args.length > 0) Secret.initFromFile(args(0))
    val conf = Option(localStorage.getItem("wallet_config"))
    conf.foreach(Secret.initFromString)
  }

  private var walletList: Seq[Wallet] = Seq.empty

  def initWallets(): Unit = {
    walletList = Seq( // TODO: properly parse config
      Wallet.fromXpub(BTC, Secret.get[String]("XpubBTC"), "btc-bitcore2.trezor.io"/*"insight.bitpay.com"*/),
      Wallet.fromXpub(BCH, Secret.get[String]("XpubBCH"), "bch-bitcore2.trezor.io"/*"cashexplorer.bitcoin.com"*/),
      Wallet.fromXpub(BTG, Secret.get[String]("XpubBTG"), "btg-bitcore2.trezor.io"),
      Wallet.fromXpub(LTC, Secret.get[String]("XpubLTC"), "ltc-bitcore2.trezor.io"/*"chain.so"*/),
      Wallet.fromXpub(ETH, Secret.get[String]("XpubETH"), "api.ethplorer.io"/*api.etherscan.io"*/),
      Wallet.fromXpub(ETC, Secret.get[String]("XpubETC"), "api.gastracker.io"/*"etcchain.com"*/),
      Wallet.fromAddressList(ADA, Secret.getArray[String]("AddressListADA"), "cardanoexplorer.com"),
      Wallet.fromExchange(Seq(IOTA, TRX), "api.binance.com", Secret.get[Credentials]("BinanceAPI")),
      Wallet.fromConstant(XMR, Some(0.58325139))
    ).flatten
  }

  def refresh(): Unit = {
    val fBalances = walletList.flatMap(_.getBalances.map { case (k, v) =>
      for {
        b <- v
        u <- b.inUSD
        c <- u.inCZK
      } println(s"$b, $u, $c")
      v
    })

    val fValues = fBalances.map { fb =>
      for {
        b <- fb
        btc <- b.inBTC
      } yield btc.value
    }

    val fSum = Future.reduce(fValues) { (b1, b2) =>
      if (b1.isNaN) b2
      else if (b2.isNaN) b1
      else b1 + b2
    }

    val fPrint = fSum.flatMap { sum =>
      val total = Balance(sum, BTC)
      for {
        totalUSD <- total.inUSD
        totalCZK <- total.inCZK
      } yield println(s"\ntotal: $total, $totalUSD, $totalCZK")
    }
  }

  def saveAndApplyConfig(): Unit = {
    localStorage.setItem("wallet_config", jQuery("#config").value().toString)
    loadConfig()
    initWallets()
  }

  def restoreConfig(): Unit = {
    Option(localStorage.getItem("wallet_config")).foreach(jQuery("#config").value)
  }

  def setupUI(): Unit = {
    jQuery("body").append(
      """<p><textarea id="config" rows="16" cols = "128"></textarea></p>
        |<p>
        | <button id="refresh">Refresh</button>
        | <button id="save_config">Save & apply config</button>
        | <button id="restore_config">Restore config</button>
        |</p>
      """.stripMargin
    )

    jQuery("#refresh").click(() => refresh())
    jQuery("#save_config").click(() => saveAndApplyConfig())
    jQuery("#restore_config").click(() => restoreConfig())

    restoreConfig()
  }

  def main(args: Array[String]): Unit = {
    loadConfig()
    initWallets()
    refresh()

    jQuery(() => setupUI())
  }

//  Await.ready(fPrint, Duration.Inf)
//  Thread.sleep(1000)
}
