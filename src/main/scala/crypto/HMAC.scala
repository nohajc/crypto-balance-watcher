package crypto

import utils.HexUtils._

class HMAC(hashAlgo: HashAlgo) {
  private val opad = Array.fill[Byte](hashAlgo.blockSize)(0x5c.toByte)
  private val ipad = Array.fill[Byte](hashAlgo.blockSize)(0x36.toByte)

  def apply(secret: String, msg: String): String = {
    val key =
      if (secret.length > 64)
        hashAlgo(secret.getBytes)
      else
        secret.getBytes

    val keyp = key ++ Array.fill[Byte](key.length % hashAlgo.blockSize)(0)

    val kXORopad = keyp.zip(opad).map {case (a, b) => (a ^ b).toByte}
    val kXORipad = keyp.zip(ipad).map {case (a, b) => (a ^ b).toByte}

    hashAlgo(kXORopad ++ hashAlgo(kXORipad ++ msg.getBytes)).toHexString
  }
}

object HMAC {
  def apply(hash: HashAlgo): HMAC = new HMAC(hash)
}
