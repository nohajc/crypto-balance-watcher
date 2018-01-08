package hdwallet

import encoding.Base58Check

object AddressUtils {
  def convertToLTC3Address(addr: String): String = Base58Check.encode(5.toByte, Base58Check.decode(addr))
}
