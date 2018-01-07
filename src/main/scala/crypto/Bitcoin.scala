package crypto

object Bitcoin {
  def HASH160(data: Seq[Byte]): Seq[Byte] = RIPEMD160(SHA256(data.toArray))
  def HASH256(data: Seq[Byte]): Seq[Byte] = SHA256(SHA256(data.toArray))
}
