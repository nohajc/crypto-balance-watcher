package crypto

import utils.HexUtils._

class HMAC(hashAlgo: HashAlgo) {
  private val opad = Array.fill[Byte](hashAlgo.blockSize)(0x5c.toByte)
  private val ipad = Array.fill[Byte](hashAlgo.blockSize)(0x36.toByte)

  def apply(secret: Array[Byte], msg: Array[Byte]): Array[Byte] = {
    val key =
      if (secret.length > hashAlgo.blockSize)
        hashAlgo(secret)
      else
        secret

    val keyp = key ++ Array.fill[Byte](hashAlgo.blockSize - key.length)(0)

    val kXORopad = keyp.zip(opad).map {case (a, b) => (a ^ b).toByte}
    val kXORipad = keyp.zip(ipad).map {case (a, b) => (a ^ b).toByte}

    hashAlgo(kXORopad ++ hashAlgo(kXORipad ++ msg))
  }
}

object HMAC {
  def apply(hash: HashAlgo): HMAC = new HMAC(hash)
}
