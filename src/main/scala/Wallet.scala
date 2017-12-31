import fr.hmil.roshttp.exceptions.HttpException
import fr.hmil.roshttp.response.SimpleHttpResponse

import scala.concurrent.Future
import monix.execution.Scheduler.Implicits.global

class Wallet(source: BalanceSource, currencies: Seq[Currency]) { // TODO: multisource wallet
  require(currencies.nonEmpty)

  def getBalance(currency: Currency): Future[Balance] = source.getCurrent(currency).map(b => Balance(b , currency)).recover {
    case HttpException(e: SimpleHttpResponse) =>
      println(e.body)
      Balance(Double.NaN, currency)
  }
  def getBalance: Future[Balance] = getBalance(currencies.head)
  def getBalances: Map[Currency, Future[Balance]] =
    currencies.map(c => c -> getBalance(c)).toMap[Currency, Future[Balance]]
}

object Wallet {
  def fromXpub(currency: Currency, xpubStr: String, queryHost: String): Wallet = {
    val srcImpl = BalanceSourceImpl.fromHost(queryHost)
    val recvAddresses = currency.addressGenerator.getAddressesFromXpubString(xpubStr, BipUtils.ReceivingAddr)
    val balanceSrc = new HDWalletBalanceSource(recvAddresses, srcImpl)
    new Wallet(balanceSrc, Seq(currency))
  }

  def fromXpub(currency: Currency, opXpub: Option[String], queryHost: String): Option[Wallet] =
    opXpub.map(xpub => fromXpub(currency, xpub, queryHost))

  def fromExchange(currencies: Seq[Currency], queryHost: String, credentials: Credentials): Wallet = {
    val balanceSrc = BalanceSource.fromHost(queryHost, credentials)
    new Wallet(balanceSrc, currencies)
  }

  def fromExchange(currencies: Seq[Currency], queryHost: String, opCred: Option[Credentials]): Option[Wallet] =
    opCred.map(cred => fromExchange(currencies, queryHost, cred))

  def fromConstant(currency: Currency, balance: Double): Wallet = {
    new Wallet(new ConstantBalanceSource(balance), Seq(currency))
  }

  def fromConstant(currency: Currency, opBalance: Option[Double]): Option[Wallet] =
    opBalance.map(bal => fromConstant(currency, bal))
}
