import hdwallet._

sealed trait Currency {
  val name: String
  val short: String
  val symbol: String = null
  val prefixNotation: Boolean = false
  val addressGenerator: AddressGenerator = new DummyAddressGenerator
}

trait Fiat extends Currency

case object USD extends Fiat {
  val name = "usd"
  val short = "USD"
  override val symbol = "$"
  override val prefixNotation: Boolean = true
}

case object CZK extends Fiat {
  val name = "czk"
  val short = "CZK"
  override val symbol = "Kč"
}

case object Default extends Currency {
  val name = "default"
  val short = "Dflt"
}

case object BTC extends Currency {
  val name = "bitcoin"
  val short = "BTC"
  override val addressGenerator = new BTCAddressGenerator
}

case object BCH extends Currency {
  val name = "bitcoin-cash"
  val short = "BCH|BCC"
  override val addressGenerator = new BCHAddressGenerator
}

case object BTG extends Currency {
  val name = "bitcoin-gold"
  val short = "BTG"
  override val addressGenerator = new BTGAddressGenerator
}

case object LTC extends Currency {
  val name = "litecoin"
  val short = "LTC"
  override val addressGenerator = new LTCAddressGenerator
}

case object ETH extends Currency {
  val name = "ethereum"
  val short = "ETH"
  override val addressGenerator = new ETHAddressGenerator
}

case object ETC extends Currency {
  val name = "ethereum-classic"
  val short = "ETC"
  override val addressGenerator = new ETCAddressGenerator
}

case object ADA extends Currency {
  val name ="cardano"
  val short = "ADA"
}

case object IOTA extends Currency {
  val name = "iota"
  val short = "IOTA"
  override val symbol = "MIOTA"
}

case object XMR extends Currency {
  val name = "monero"
  val short = "XMR"
}

case object TRX extends Currency {
  val name = "tron"
  val short = "TRX"
}