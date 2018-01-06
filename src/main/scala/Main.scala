import monix.execution.Scheduler.Implicits.global

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main extends App {
  if (args.length > 0) Secret.initFromFile(args(0))

  val lst = Seq(
    Wallet.fromXpub(BTC, Secret.get[String]("XpubBTC"), "btc-bitcore2.trezor.io"/*"insight.bitpay.com"*/),
    Wallet.fromXpub(BCH, Secret.get[String]("XpubBCH"), "bch-bitcore2.trezor.io"/*"cashexplorer.bitcoin.com"*/),
    Wallet.fromXpub(BTG, Secret.get[String]("XpubBTG"), "btg-bitcore2.trezor.io"),
    Wallet.fromXpub(LTC, Secret.get[String]("XpubLTC"), "ltc-bitcore2.trezor.io"/*"chain.so"*/),
    Wallet.fromXpub(ETH, Secret.get[String]("XpubETH"), "api.ethplorer.io"/*api.etherscan.io"*/),
    Wallet.fromXpub(ETC, Secret.get[String]("XpubETC"), "api.gastracker.io"/*"etcchain.com"*/),
    Wallet.fromExchange(Seq(ADA, IOTA, TRX), "api.binance.com", Secret.get[Credentials]("BinanceAPI")),
    Wallet.fromConstant(XMR, Some(0.58325139))
  ).flatten

  val fBalances = lst.flatMap(_.getBalances.map { case (k, v) =>
    for {
      b <- v
      u <- b.inUSD
      c <- u.inCZK
    } println(s"$b, $u, $c")
    v
  })

  val fSum = fBalances.map { fb =>
    for {
      b <- fb
      btc <- b.inBTC
    } yield btc.value
  }.reduce { (fb1, fb2) =>
    for {
      b1 <- fb1
      b2 <- fb2
    } yield {
      if (b1.isNaN) b2
      else if (b2.isNaN) b1
      else b1 + b2
    }
  }

  val fPrint = fSum.flatMap {sum =>
      val total = Balance(sum, BTC)
      for {
        totalUSD <- total.inUSD
        totalCZK <- total.inCZK
      } yield println(s"\ntotal: $total, $totalUSD, $totalCZK")
  }

  Await.ready(fPrint, Duration.Inf)
  Thread.sleep(1000)
}
